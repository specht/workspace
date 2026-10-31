# A command as it may stand in the log or in an error (Main.shell_capture
# writes a failed one there): whatever looks like a secret is replaced by
# »***«. Secrets belong in the environment of the command, not in it
# (docker exec -e NAME, Workspace start); this is the net for the day one
# slips in. Plain Ruby, tested on its own (test/command_redaction_test.rb).
module CommandRedaction
    HIDDEN = '***'.freeze

    PATTERNS = [
        # --password=x, --password x, --token=…, --secret …, --api-key …
        /(--(?:password|passwd|token|secret|api[-_]?key)(?:=|\s+))(?:'[^']*'|"[^"]*"|\S+)/i,
        # mysql -pSECRET and cypher-shell -p SECRET; elsewhere -p is a port or
        # mkdir's parents and stays
        /(\bmysql(?:dump)?\b[^|;&]*?\s-p)(?:'[^']*'|"[^"]*"|[^\s'"]+)/,
        /(\bcypher-shell\b[^|;&]*?\s-p\s+)(?:'[^']*'|"[^"]*"|\S+)/,
        # NAME_PASSWORD=x, TOKEN=x, SECRET=x in an assignment or -e NAME=x
        /(\b[A-Z0-9_]*(?:PASSWORD|PASSWD|PWD|TOKEN|SECRET|API_KEY)[A-Z0-9_]*=)(?:'[^']*'|"[^"]*"|\S+)/,
        # https://user:password@host
        %r{(\w+://[^/\s:@]+:)[^@\s/]+(?=@)},
    ].freeze

    def self.redact(command)
        PATTERNS.reduce(command.to_s) { |text, pattern| text.gsub(pattern) { "#{$1}#{HIDDEN}" } }
    end
end
