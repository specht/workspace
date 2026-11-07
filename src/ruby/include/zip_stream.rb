# The streaming ZIP of folders (tools/zip_stream.py), for »Meine Daten
# herunterladen« (accounts_page.rb) and the results of a
# Leistungsüberprüfung (exams_page.rb): the command, run as the Workspace
# user, so that a link a student planted reaches nothing more than the
# student could read. One path for both: moved from main.rb into include/,
# a path built from __dir__ pointed into include/tools and the export was
# empty from 31 October to 7 November 2026 (test/zip_stream_test.rb).
module ZipStream
    PATH = File.expand_path('../tools/zip_stream.py', __dir__)

    def self.command(uid, gid, *args)
        ['setpriv', "--reuid=#{uid}", "--regid=#{gid}", '--clear-groups', 'python3', '-I', PATH] + args
    end

    # A folder per person in an archive: the name as on the printout,
    # without what a file system refuses (and no »=«, which ends the prefix
    # for zip_stream.py), unique without regard to case.
    def self.folder_names(names)
        used = Hash.new(0)
        names.map do |name|
            base = name.to_s.gsub(%r{[/\\:*?"<>|=\x00-\x1f]}, ' ').squeeze(' ').strip.sub(/\A\.+/, '').strip
            base = 'ohne Namen' if base.empty?
            used[base.downcase] += 1
            used[base.downcase] == 1 ? base : "#{base} (#{used[base.downcase]})"
        end
    end
end
