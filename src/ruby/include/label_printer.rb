# Labels for the Tag der offenen Tür (/etiketten, docs/etiketten.md):
# visitors draw on a label or put stamps and a line of text on it, on their
# phone (a QR code at the stand) or a computer there; the server turns the
# picture into ZPL, and a small script on a laptop at school
# (src/scripts/label-station.rb) fetches the labels and writes them to the
# label printer on its USB port. The Workspace's server cannot reach the
# printer, so the laptop asks.
#
# A label is a picture of WIDTH × HEIGHT dots, black or white, sent as
# hexadecimal digits, two per byte, eight dots per byte, the leftmost dot in
# the highest bit, row after row: exactly the data of ZPL's ^GFA. The size
# and the shift of the print (HOME) are those of the old Hackschule's
# printer: 57 × 31.5 mm labels at 8 dots per mm, with a millimetre more
# around. Plain Ruby, tested on its own (test/label_printer_test.rb); the
# server's part is in label_printer_api.rb.
module LabelPrinter
    WIDTH = 472
    HEIGHT = 268
    ROW_BYTES = WIDTH / 8
    BYTES = ROW_BYTES * HEIGHT
    HOME = '^LT0^LS-189^LH0,0'
    # the safe area: what lies closer to the border may be cut off
    MARGIN = 16

    HOURS = [2, 4, 8, 12].freeze      # how long the switch stays on
    BROWSER_SECONDS = 20              # between two labels of one browser
    MAX_QUEUED = 15                   # labels waiting at once
    KEEP_SECONDS = 2 * 86_400         # labels go after two days
    PHOTO_SECONDS = 2 * 3600          # a label with a photo: gone when printed, or after two hours
    STATION_SECONDS = 15              # the station counts as there this long after asking

    # The picture as the page sends it: BYTES * 2 hexadecimal digits
    def self.valid_bits?(hex)
        hex.is_a?(String) && hex.size == BYTES * 2 && hex.match?(/\A\h+\z/)
    end

    # Not all white: an empty label is no label
    def self.blank?(hex) = !hex.match?(/[1-9a-fA-F]/)

    # The label as ZPL for the printer, as the old Hackschule sent it (no
    # ^PW: the printer keeps its own width; the station can change HOME)
    def self.zpl(hex)
        "^XA#{HOME}\n^FO0,0^GFA,#{BYTES},#{BYTES},#{ROW_BYTES},#{hex.upcase}^FS\n^XZ\n"
    end

    # Whether the switch is on: { until: } with a time still ahead
    def self.on?(setting, now = Time.now.to_i) = !setting.nil? && setting[:until].to_i > now

    # Whether a browser may print again: its last label at least
    # BROWSER_SECONDS ago. last: { browser => time }
    def self.browser_wait(last, browser, now = Time.now.to_i)
        [BROWSER_SECONDS - (now - last[browser].to_i), 0].max
    end
end
