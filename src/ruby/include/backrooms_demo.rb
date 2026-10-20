# The finished level of the Backrooms (»Nachtschicht«, a clone of
# specht/backrooms-nachtschicht inside specht/backrooms-starter) as a
# website with its baked light, for students to walk through, also with a
# VR headset. It is never in this repository: export.sh in the starter
# builds the folder (the engine, the level with its light and models), and
# the maintainer copies it with rsync to INTERNAL_PATH/nachtschicht on the
# server (/mnt/hackschule/workspace/internal/nachtschicht), seen here as
# /internal/nachtschicht (docs/entwicklung.md). Served without a login under
# PREFIX, linked only from the end of the hidden activity Level 3.
module BackroomsDemo
    PREFIX = 'nachtschicht-8ee59051'
    ROOT = '/internal/nachtschicht'

    MIME = {
        '.html' => 'text/html; charset=utf-8', '.js' => 'application/javascript; charset=utf-8',
        '.css' => 'text/css; charset=utf-8', '.ttf' => 'font/ttf', '.png' => 'image/png',
        '.jpg' => 'image/jpeg', '.jpeg' => 'image/jpeg', '.webp' => 'image/webp',
        '.gltf' => 'model/gltf+json', '.glb' => 'model/gltf-binary', '.bin' => 'application/octet-stream',
        '.ini' => 'text/plain; charset=utf-8', '.room' => 'text/plain; charset=utf-8',
        '.light' => 'application/octet-stream', '.json' => 'application/json',
    }.freeze

    # The file for a path below PREFIX ('' is the page), or nil: only files
    # inside ROOT with a known type, no hidden files.
    def self.file(rel, root: ROOT)
        rel = rel.to_s
        rel = 'index.html' if rel.empty?
        return nil if rel.start_with?('/') || rel.split('/').any? { |part| part.empty? || part.start_with?('.') }
        return nil unless MIME.key?(File.extname(rel).downcase)
        return nil unless File.directory?(root)
        path = File.join(root, rel)
        return nil unless File.file?(path) && File.realpath(path).start_with?("#{File.realpath(root)}/")
        path
    end

    def self.mime(path)
        MIME[File.extname(path).downcase]
    end
end
