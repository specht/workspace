def colored(text, color: nil, bg: nil, bold: false, underline: false)
    codes = []

    colors = {
        black: 30, red: 31, green: 32, yellow: 33, blue: 34,
        magenta: 35, cyan: 36, white: 37, gray: 90
    }

    bg_colors = {
        black: 40, red: 41, green: 42, yellow: 43, blue: 44,
        magenta: 45, cyan: 46, white: 47
    }

    codes << colors[color.to_sym] if color && colors[color.to_sym]
    codes << bg_colors[bg.to_sym] if bg && bg_colors[bg.to_sym]
    codes << 1 if bold
    codes << 4 if underline

    start = "\e[#{codes.join(';')}m"
    reset = "\e[0m"

    "#{start}#{text}#{reset}"
end

def confirm(prompt = "Do you want to proceed? (j/n): ")
    print "#{prompt} "
    input = gets.chomp.strip

    until input.match?(/\A[Jn]\z/i)
        print "Bitte gib j (ja) oder n (nein) ein: "
        input = gets.chomp.strip
    end

    input.downcase == "j"
end

def run_with_scrolling_tail(cmd, line_limit: 8)
    lines = []
    IO.popen(cmd, err: [:child, :out]) do |io|
        io.each_line do |line|
            lines << line.chomp
            print "\e[#{lines.size - 1}A" unless lines.size == 1
            lines.shift if lines.size > line_limit
            lines.each { |l| puts l.ljust(IO.console.winsize[1]) }
            # sleep 0.1
                # puts line.ljust(IO.console.winsize[1]).strip
        end
    end
    # Process.wait
    status = $?.exitstatus
    # print "\e[#{lines.size}A"
    # lines.each { puts " " * IO.console.winsize[1] }
    # print "\e[#{lines.size}A"
    if status != 0
        puts "❌ Ups, etwas ging schief:\nBefehl: #{cmd}\nStatus: #{status}"
        exit status
    end
end