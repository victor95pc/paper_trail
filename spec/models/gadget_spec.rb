require "rails_helper"

describe Gadget, type: :model do
  it { is_expected.to be_versioned }

  let(:gadget) { Gadget.create!(name: "Wrench", brand: "Acme") }

  describe "updates", versioning: true do
    it "should generate a version for updates to `name` attribute" do
      expect { gadget.update_attribute(:name, "Hammer").to change { gadget.versions.size }.by(1) }
    end

    it "should ignore for updates to `brand` attribute" do
      expect { gadget.update_attribute(:brand, "Stanley") }.to_not change { gadget.versions.size }
    end

    it "should still generate a version when only the `updated_at` attribute is updated" do
      # Plus 1 second because MySQL lacks sub-second resolution
      expect {
        gadget.update_attribute(:updated_at, Time.now + 1)
      }.to change { gadget.versions.size }.by(1)
    end
  end

  describe "Methods" do
    describe "Instance", versioning: true do
      describe "private" do
        describe '#changed_notably?' do
          subject { Gadget.new(created_at: Time.now) }

          context "create events" do
            it { expect(subject.send(:changed_notably?)).to be true }
          end

          context "update events" do
            before { subject.save! }

            context "without update timestamps" do
              it "should only acknowledge non-ignored attrs" do
                subject.name = "Wrench"
                expect(subject.send(:changed_notably?)).to be true
              end

              it "should not acknowledge ignored attr (brand)" do
                subject.brand = "Acme"
                expect(subject.send(:changed_notably?)).to be false
              end
            end

            context "with update timestamps" do
              it "should only acknowledge non-ignored attrs" do
                subject.name = "Wrench"
                subject.updated_at = Time.now
                expect(subject.send(:changed_notably?)).to be true
              end

              it "should not acknowledge ignored attrs and timestamps only" do
                subject.brand = "Acme"
                subject.updated_at = Time.now
                expect(subject.send(:changed_notably?)).to be false
              end
            end
          end
        end
      end
    end
  end
end
