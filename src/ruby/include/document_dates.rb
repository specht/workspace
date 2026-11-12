require 'date'
require 'json'
require 'open3'

# Dates in the starting file of a document task, filled in on download
# (docs/aufgaben.md, »Dokumente«; the maintainer, 12 November 2026): a
# letter dated »2. März« read in November looks forgotten. The task names
# the dates of its starting file and what each becomes,
#
#   check:
#     type: document
#     file: formatting-marks/01 Einladung.docx
#     dates:
#       '2. März': heute          # the day of the download
#       '9. März': Dienstag       # the first Tuesday at least a week later
#
# and /api/task/document_starter/… hands out a copy with them replaced
# (docx_replace.py). The weekday is the one the text names beside the date
# (»am Dienstag, dem 9. März«), so the sentence stays true.
#
# A student may download on one day and check on another, at home; the
# check therefore reads any German date where the starting file has one
# (pattern, normalize: DocumentCheck's text, align and bold rules).
module DocumentDates
    MONTHS = %w[Januar Februar März April Mai Juni Juli August September Oktober November Dezember].freeze
    WEEKDAYS = %w[Sonntag Montag Dienstag Mittwoch Donnerstag Freitag Samstag].freeze
    TODAY = 'heute'
    # »2. März«, »14. November«: as the starting files write a date, no year
    DATE = /(?<![0-9])[0-9]{1,2}\. (?:#{MONTHS.join('|')})(?![[:alpha:]])/
    # what stands for any date when texts are compared (letters only, so
    # that the comparison of words keeps it whole)
    TOKEN = 'XDATUMX'
    SCRIPT = File.join(__dir__, 'docx_replace.py')

    def self.spec_errors(dates)
        return ['dates must map dates of the starting file (2. März) to heute or a weekday (Dienstag)'] unless dates.is_a?(Hash) && !dates.empty?

        dates.flat_map do |old, new|
            errors = []
            errors << "dates: #{old.inspect} is not a date like 2. März" unless old.is_a?(String) && old.match?(/\A#{DATE}\z/)
            errors << "dates: #{new.inspect} must be heute or a weekday (#{WEEKDAYS.join(', ')})" unless new == TODAY || WEEKDAYS.include?(new)
            errors
        end
    end

    def self.format(date)
        "#{date.day}. #{MONTHS[date.month - 1]}"
    end

    # The dates of the starting file and what they become on this day:
    # heute is the day itself, a weekday the first such day at least a
    # week later.
    def self.resolve(dates, today = Date.today)
        dates.to_h do |old, new|
            if new == TODAY
                [old, format(today)]
            else
                day = today + 7
                day += 1 until day.wday == WEEKDAYS.index(new)
                [old, format(day)]
            end
        end
    end

    # The starting file with its dates filled in, as bytes; nil if that
    # did not work (the plain file is better than none).
    def self.fill(path, dates, today = Date.today)
        out, status = Open3.capture2('timeout', '20', 'python3', '-I', SCRIPT, path,
                                     :stdin_data => JSON.generate(resolve(dates, today)), :binmode => true)
        status.success? && !out.empty? ? out : nil
    rescue SystemCallError
        nil
    end

    # A text with every date as the same word, to compare texts that differ
    # only in their dates.
    def self.normalize(text)
        text.to_s.gsub(DATE, TOKEN)
    end

    # A piece of text from a rule (»14. März«) as a pattern that finds it
    # with any date in place of its date.
    def self.pattern(piece)
        Regexp.new(piece.to_s.split(DATE, -1).map { |part| Regexp.escape(part) }.join(DATE.source))
    end

    def self.date?(piece)
        piece.to_s.match?(/\A#{DATE}\z/)
    end
end
