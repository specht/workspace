# The VS Code settings a person's Workspace starts with
# (default-vscode-settings.json), merged into their own settings.json each
# time the Workspace starts.
#
# - A setting the person does not have yet gets the default; true/false
#   only if the key is missing, so »off« stays off.
# - An object setting (files.exclude) gets the default entries the person
#   does not have yet; their own entries stay.
# - A setting that still has an earlier default (superseded-vscode-settings
#   .json) gets the current one: the person never changed it, so they get
#   what everyone gets now. A value they chose themselves stays.
module VscodeSettings
    def self.merge(user, defaults, superseded = {})
        user = user.dup
        defaults.each_pair do |key, value|
            if value == true || value == false
                user[key] = value unless user.include?(key)
            elsif value.is_a?(Hash) && user[key].is_a?(Hash)
                user[key] = value.reject { |k, _| user[key].include?(k) }.merge(user[key])
            elsif user.include?(key) && Array(superseded[key]).include?(user[key])
                user[key] = value
            else
                user[key] = value if user[key].nil?
            end
        end
        user
    end
end
