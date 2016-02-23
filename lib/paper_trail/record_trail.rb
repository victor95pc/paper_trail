module PaperTrail
  # Represents the "paper trail" for a single record.
  class RecordTrail
    def initialize(record)
      @record = record
    end

    # Utility method for reifying. Anything executed inside the block will
    # appear like a new record.
    def appear_as_new_record
      @record.instance_eval {
        alias :old_new_record? :new_record?
        alias :new_record? :present?
      }
      yield
      @record.instance_eval { alias :new_record? :old_new_record? }
    end

    def attributes_before_change
      changed = @record.changed_attributes.select { |k, _v|
        @record.class.column_names.include?(k)
      }
      @record.attributes.merge(changed)
    end

    # Invoked after rollbacks to ensure versions records are not created for
    # changes that never actually took place. Optimization: Use lazy `reset`
    # instead of eager `reload` because, in many use cases, the association will
    # not be used.
    def clear_rolled_back_versions
      versions.reset
    end

    # Invoked via`after_update` callback for when a previous version is
    # reified and then saved.
    def clear_version_instance
      @record.send("#{@record.class.version_association_name}=", nil)
    end

    # @api private
    def changes
      notable_changes = @record.changes.delete_if { |k, _v|
        !@record.notably_changed.include?(k)
      }
      AttributeSerializers::ObjectChangesAttribute.
        new(@record.class).
        serialize(notable_changes)
      notable_changes.to_hash
    end

    def enabled_for_model?
      @record.class.paper_trail.enabled?
    end

    # Returns true if this instance is the current, live one;
    # returns false if this instance came from a previous version.
    def live?
      source_version.nil?
    end

    # @api private
    def merge_metadata(data)
      # First we merge the model-level metadata in `meta`.
      @record.paper_trail_options[:meta].each do |k, v|
        data[k] =
          if v.respond_to?(:call)
            v.call(@record)
          elsif v.is_a?(Symbol) && @record.respond_to?(v, true)
            # If it is an attribute that is changing in an existing object,
            # be sure to grab the current version.
            if @record.has_attribute?(v) &&
                @record.send("#{v}_changed?".to_sym) &&
                data[:event] != "create"
              @record.send("#{v}_was".to_sym)
            else
              @record.send(v)
            end
          else
            v
          end
      end

      # Second we merge any extra data from the controller (if available).
      data.merge(PaperTrail.controller_info || {})
    end

    # Returns the object (not a Version) as it became next.
    # NOTE: if self (the item) was not reified from a version, i.e. it is the
    #  "live" item, we return nil.  Perhaps we should return self instead?
    def next_version
      subsequent_version = source_version.next
      subsequent_version ? subsequent_version.reify : @record.class.find(@record.id)
    rescue # TODO: Rescue something more specific
      nil
    end

    # Returns hash of attributes (with appropriate attributes serialized),
    # omitting attributes to be skipped.
    def object_attrs_for_paper_trail
      attrs = attributes_before_change.except(*@record.paper_trail_options[:skip])
      AttributeSerializers::ObjectAttribute.new(@record.class).serialize(attrs)
      attrs
    end

    # Returns who put `@record` into its current state.
    def originator
      (source_version || versions.last).try(:whodunnit)
    end

    # Returns the object (not a Version) as it was most recently.
    def previous_version
      (source_version ? source_version.previous : versions.last).try(:reify)
    end

    def record_create
      return unless @record.paper_trail_switched_on?
      data = {
        event: @record.paper_trail_event || "create",
        whodunnit: PaperTrail.whodunnit
      }
      if @record.respond_to?(:updated_at)
        data[PaperTrail.timestamp_field] = @record.updated_at
      end
      if record_object_changes? && @record.changed_notably?
        data[:object_changes] = recordable_object_changes
      end
      @record.add_transaction_id_to(data)
      versions_assoc = @record.send(@record.class.versions_association_name)
      version = versions_assoc.create! merge_metadata(data)
      @record.update_transaction_id(version)
      save_associations(version)
    end

    def record_destroy
      if @record.paper_trail_switched_on? && !@record.new_record?
        data = {
          item_id: @record.id,
          item_type: @record.class.base_class.name,
          event: @record.paper_trail_event || "destroy",
          object: recordable_object,
          whodunnit: PaperTrail.whodunnit
        }
        @record.add_transaction_id_to(data)
        version = @record.class.paper_trail.version_class.create(merge_metadata(data))
        if version.errors.any?
          @record.log_version_errors(version, :destroy)
        else
          @record.send("#{@record.class.version_association_name}=", version)
          @record.send(@record.class.versions_association_name).reset
          @record.update_transaction_id(version)
          save_associations(version)
        end
      end
    end

    # Returns a boolean indicating whether to store serialized version diffs
    # in the `object_changes` column of the version record.
    # @api private
    def record_object_changes?
      @record.paper_trail_options[:save_changes] &&
        @record.class.paper_trail.version_class.column_names.include?("object_changes")
    end

    def record_update(force)
      if @record.paper_trail_switched_on? && (force || @record.changed_notably?)
        data = {
          event: @record.paper_trail_event || "update",
          object: recordable_object,
          whodunnit: PaperTrail.whodunnit
        }
        if @record.respond_to?(:updated_at)
          data[PaperTrail.timestamp_field] = @record.updated_at
        end
        if record_object_changes?
          data[:object_changes] = recordable_object_changes
        end
        @record.add_transaction_id_to(data)
        versions_assoc = @record.send(@record.class.versions_association_name)
        version = versions_assoc.create(merge_metadata(data))
        if version.errors.any?
          @record.log_version_errors(version, :update)
        else
          @record.update_transaction_id(version)
          save_associations(version)
        end
      end
    end

    # Returns an object which can be assigned to the `object` attribute of a
    # nascent version record. If the `object` column is a postgres `json`
    # column, then a hash can be used in the assignment, otherwise the column
    # is a `text` column, and we must perform the serialization here, using
    # `PaperTrail.serializer`.
    # @api private
    def recordable_object
      if @record.class.paper_trail.version_class.object_col_is_json?
        object_attrs_for_paper_trail
      else
        PaperTrail.serializer.dump(object_attrs_for_paper_trail)
      end
    end

    # Returns an object which can be assigned to the `object_changes`
    # attribute of a nascent version record. If the `object_changes` column is
    # a postgres `json` column, then a hash can be used in the assignment,
    # otherwise the column is a `text` column, and we must perform the
    # serialization here, using `PaperTrail.serializer`.
    # @api private
    def recordable_object_changes
      if @record.class.paper_trail.version_class.object_changes_col_is_json?
        changes
      else
        PaperTrail.serializer.dump(changes)
      end
    end

    # Invoked via callback when a user attempts to persist a reified
    # `Version`.
    def reset_timestamp_attrs_for_update_if_needed
      return if live?
      @record.send(:timestamp_attributes_for_update_in_model).each do |column|
        # ActiveRecord 4.2 deprecated `reset_column!` in favor of
        # `restore_column!`.
        if @record.respond_to?("restore_#{column}!")
          @record.send("restore_#{column}!")
        else
          @record.send("reset_#{column}!")
        end
      end
    end

    # Saves associations if the join table for `VersionAssociation` exists.
    def save_associations(version)
      return unless PaperTrail.config.track_associations?
      save_associations_belongs_to(version)
      save_associations_habtm(version)
    end

    def save_associations_belongs_to(version)
      @record.class.reflect_on_all_associations(:belongs_to).each do |assoc|
        assoc_version_args = {
          version_id: version.id,
          foreign_key_name: assoc.foreign_key
        }

        if assoc.options[:polymorphic]
          associated_record = @record.send(assoc.name) if @record.send(assoc.foreign_type)
          if associated_record && associated_record.class.paper_trail.enabled?
            assoc_version_args[:foreign_key_id] = associated_record.id
          end
        elsif assoc.klass.paper_trail.enabled?
          assoc_version_args[:foreign_key_id] = @record.send(assoc.foreign_key)
        end

        if assoc_version_args.key?(:foreign_key_id)
          PaperTrail::VersionAssociation.create(assoc_version_args)
        end
      end
    end

    def save_associations_habtm(version)
      # Use the :added and :removed keys to extrapolate the HABTM associations
      # to before any changes were made
      @record.class.reflect_on_all_associations(:has_and_belongs_to_many).each do |a|
        next unless
          @record.class.paper_trail_save_join_tables.include?(a.name) ||
              a.klass.paper_trail.enabled?
        assoc_version_args = {
          version_id: version.transaction_id,
          foreign_key_name: a.name
        }
        assoc_ids =
          @record.send(a.name).to_a.map(&:id) +
          (@record.paper_trail_habtm.try(:[], a.name).try(:[], :removed) || []) -
          (@record.paper_trail_habtm.try(:[], a.name).try(:[], :added) || [])
        assoc_ids.each do |id|
          PaperTrail::VersionAssociation.create(assoc_version_args.merge(foreign_key_id: id))
        end
      end
    end

    def source_version
      version
    end

    # Mimics the `touch` method from `ActiveRecord::Persistence`, but also
    # creates a version. A version is created regardless of options such as
    # `:on`, `:if`, or `:unless`.
    #
    # TODO: look into leveraging the `after_touch` callback from
    # `ActiveRecord` to allow the regular `touch` method to generate a version
    # as normal. May make sense to switch the `record_update` method to
    # leverage an `after_update` callback anyways (likely for v4.0.0)
    def touch_with_version(name = nil)
      unless @record.persisted?
        raise ActiveRecordError, "can not touch on a new record object"
      end
      attributes = @record.send :timestamp_attributes_for_update_in_model
      attributes << name if name
      current_time = @record.send :current_time_from_proper_timezone
      attributes.each { |column|
        @record.send(:write_attribute, column, current_time)
      }
      @record.record_update(true) unless will_record_after_update?
      @record.save!(validate: false)
    end

    # Returns the object (not a Version) as it was at the given timestamp.
    def version_at(timestamp, reify_options = {})
      # Because a version stores how its object looked *before* the change,
      # we need to look for the first version created *after* the timestamp.
      v = versions.subsequent(timestamp, true).first
      return v.reify(reify_options) if v
      @record unless @record.destroyed?
    end

    # Returns the objects (not Versions) as they were between the given times.
    def versions_between(start_time, end_time)
      versions = send(@record.class.versions_association_name).between(start_time, end_time)
      versions.collect { |version|
        version_at(version.send(PaperTrail.timestamp_field))
      }
    end

    # Executes the given method or block without creating a new version.
    def without_versioning(method = nil)
      paper_trail_was_enabled = enabled_for_model?
      @record.class.paper_trail.disable
      if method
        if respond_to?(method)
          public_send(method)
        else
          @record.send(method)
        end
      else
        yield @record
      end
    ensure
      @record.class.paper_trail.enable if paper_trail_was_enabled
    end

    # Temporarily overwrites the value of whodunnit and then executes the
    # provided block.
    def whodunnit(value)
      raise ArgumentError, "expected to receive a block" unless block_given?
      current_whodunnit = PaperTrail.whodunnit
      PaperTrail.whodunnit = value
      yield @record
    ensure
      PaperTrail.whodunnit = current_whodunnit
    end

    private

    # Returns true if `save` will cause `record_update`
    # to be called via the `after_update` callback.
    def will_record_after_update?
      on = @record.paper_trail_options[:on]
      on.nil? || on.include?(:update)
    end

    def version
      @record.public_send(@record.class.version_association_name)
    end

    def versions
      @record.public_send(@record.class.versions_association_name)
    end
  end
end
