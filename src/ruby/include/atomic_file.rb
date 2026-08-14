require 'securerandom'

module AtomicFile
    def self.write(path, contents)
        directory = File.dirname(path)
        basename = File.basename(path)
        temporary_path = File.join(
            directory,
            ".#{basename}.#{Process.pid}.#{Thread.current.object_id}.#{SecureRandom.hex(4)}.tmp"
        )

        File.open(temporary_path, 'wb') do |file|
            file.write(contents)
            file.flush
            file.fsync
        end
        File.rename(temporary_path, path)
    ensure
        File.delete(temporary_path) if temporary_path && File.exist?(temporary_path)
    end
end
