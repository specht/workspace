require 'cgi'
require 'date'
require 'digest'

# »Wo du angemeldet bist« in the profile: the browsers in which an account is
# logged in, each with the day of the login, the day it was last used and a
# coarse label of the device (»Firefox auf Windows«), and a button to end it.
# Plain Ruby, tested on its own; login_sessions_page.rb wires it into Main.
#
# Stored per Session node, as little as needed (Privacy, »Sitzungen«):
# - created_on: the day of the login, without a time;
# - last_used_on: the day it was last used, written at most once a day, not
#   while a teacher looks in (the before filter in main.rb);
# - device: the label below, made from the User-Agent at the login. The
#   User-Agent itself and the IP address are never stored.
# Sessions from before this have none of the three and say so.
#
# The page never sees a sid (the login secret): a session is named by
# public_id, a hash of its sid that cannot be turned back into it.
module LoginSessions
    UNKNOWN_BROWSER = 'unbekannter Browser'
    # After »auf«: »Firefox auf unbekanntem System«.
    UNKNOWN_SYSTEM = 'unbekanntem System'

    # In this order: Edge, Opera and Samsung Internet say »Chrome« too, and
    # Chrome says »Safari«. Mobile variants have their own tokens (CriOS is
    # Chrome on iOS, FxiOS Firefox, EdgiOS/EdgA Edge, OPT/OPR Opera).
    BROWSERS = [
        ['Edge', /\b(?:Edg|EdgA|EdgiOS|Edge)\//],
        ['Opera', /\b(?:OPR|OPT|Opera)\b/],
        ['Samsung Internet', /\bSamsungBrowser\//],
        ['Firefox', /\b(?:Firefox|FxiOS)\//],
        ['Chrome', /\b(?:Chrome|HeadlessChrome|CriOS|Chromium)\//],
        ['Safari', /\bVersion\/[\d.]+.*\bSafari\//],
    ].freeze

    # Android and ChromeOS before Linux (both say »Linux«), iOS before macOS
    # (»like Mac OS X«).
    SYSTEMS = [
        ['iPad', /\biPad\b/],
        ['iPhone', /\b(?:iPhone|iPod)\b/],
        ['Android', /\bAndroid\b/],
        ['ChromeOS', /\bCrOS\b/],
        ['Windows', /\bWindows\b/],
        ['Mac', /\bMacintosh\b|\bMac OS X\b/],
        ['Linux', /\bLinux\b|\bX11\b/],
    ].freeze

    # »Firefox auf Windows«, »Safari auf Mac oder iPad«, »Chrome auf Android«.
    def self.device_label(user_agent)
        ua = user_agent.to_s[0, 1024]
        browser = BROWSERS.find { |_, re| ua.match?(re) }&.first || UNKNOWN_BROWSER
        system = SYSTEMS.find { |_, re| ua.match?(re) }&.first
        # An iPad asks for the desktop pages and says it is a Mac; only
        # Safari (and the iOS browsers) can be on either.
        if system == 'Mac' && (browser == 'Safari' || ua.match?(/\b(?:CriOS|FxiOS|EdgiOS|OPT)\//))
            system = 'Mac oder iPad'
        end
        # Android without »Mobile« is a tablet.
        system = 'Android-Tablet' if system == 'Android' && !ua.include?('Mobile')
        "#{browser} auf #{system || UNKNOWN_SYSTEM}"
    end

    ICONS = {
        'iPhone' => 'bi-phone',
        'Android' => 'bi-phone',
        'iPad' => 'bi-tablet',
        'Android-Tablet' => 'bi-tablet',
        'Mac' => 'bi-laptop',
        'Mac oder iPad' => 'bi-laptop',
        'ChromeOS' => 'bi-laptop',
        'Windows' => 'bi-pc-display',
        'Linux' => 'bi-pc-display',
    }.freeze

    # The icon for a stored label; a session without one gets a question mark.
    def self.icon(device)
        return 'bi-question-circle' if device.to_s.empty?

        ICONS[device.to_s.split(' auf ', 2)[1]] || 'bi-display'
    end

    # The fields a new Session node gets at the login.
    def self.new_session_fields(user_agent, today: Date.today)
        { :created_on => today.to_s, :last_used_on => today.to_s, :device => device_label(user_agent) }
    end

    # The name of a session on the page; the sid never leaves the server.
    def self.public_id(sid)
        Digest::SHA256.hexdigest("login-session:#{sid}")[0, 32]
    end

    def self.valid_public_id?(id)
        id.is_a?(String) && id.match?(/\A[0-9a-f]{32}\z/)
    end

    MONTHS = %w[Januar Februar März April Mai Juni Juli August September Oktober November Dezember].freeze

    def self.date(value)
        Date.iso8601(value.to_s)
    rescue ArgumentError, TypeError
        nil
    end

    # »heute«, »gestern«, »am 3. Oktober 2026«.
    def self.day_text(value, today: Date.today)
        day = date(value)
        return nil unless day
        return 'heute' if day == today
        return 'gestern' if day == today - 1

        "am #{day.day}. #{MONTHS[day.month - 1]} #{day.year}"
    end

    # »angemeldet am 3. Oktober 2026 · zuletzt gestern«; the last use only
    # when it is another day than the login.
    def self.when_text(session, today: Date.today)
        created = day_text(session[:created_on], :today => today)
        last = day_text(session[:last_used_on], :today => today)
        parts = [created ? "angemeldet #{created}" : 'Tag der Anmeldung unbekannt']
        parts << "zuletzt #{last}" if last && session[:last_used_on].to_s != session[:created_on].to_s
        parts.join(' · ')
    end

    # The person's own sessions, this browser first, then the most recently
    # used. sessions: Hashes with :sid, :created_on, :last_used_on, :device,
    # :impersonated_by (a teacher looking in: never listed).
    def self.listed(sessions, current_sid:)
        own = sessions.reject { |s| s[:impersonated_by] || s[:sid].to_s.empty? }
        own.sort_by do |s|
            [s[:sid] == current_sid ? 0 : 1, -(date(s[:last_used_on]) || Date.new(0)).jd,
             -(date(s[:created_on]) || Date.new(0)).jd, public_id(s[:sid])]
        end
    end

    # Which sid to end for a request with id (from the page): [:ok, sid], or
    # [:impersonating], [:invalid], [:not_found] (not one of the owner's
    # sessions, or one a teacher uses to look in), [:current] (this browser
    # logs out with /logout). sessions: as in listed, each with :email.
    def self.sid_to_end(sessions, id, owner:, current_sid:, impersonating:)
        return [:impersonating] if impersonating
        return [:invalid] unless valid_public_id?(id)

        session = sessions.find do |s|
            s[:email].to_s.downcase == owner.to_s.downcase && s[:impersonated_by].nil? &&
                !s[:sid].to_s.empty? && public_id(s[:sid]) == id
        end
        return [:not_found] unless session
        return [:current] if session[:sid] == current_sid

        [:ok, session[:sid]]
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.row_html(session, current:, today: Date.today)
        device = session[:device].to_s.empty? ? 'Unbekanntes Gerät' : session[:device].to_s
        device = device[0].upcase + device[1..] if device.start_with?('unbekannt')
        badge = current ? " <span class='badge text-bg-primary'>dieser Browser</span>" : ''
        # On a phone only the icon (profil.html), so the label is the name.
        content = "<i class='bi bi-box-arrow-right'></i><span class='login-session-label'>Abmelden</span>"
        button = if current
            "<a class='btn btn-sm btn-outline-secondary' href='/logout' title='Abmelden' aria-label='In diesem Browser abmelden'>#{content}</a>"
        else
            "<button type='button' class='btn btn-sm btn-outline-secondary bu-end-session' title='Abmelden' aria-label='#{h("Dort abmelden: #{device}")}'>#{content}</button>"
        end
        "<li class='login-session#{current ? ' current' : ''}' data-id='#{public_id(session[:sid])}'>" \
            "<i class='bi #{icon(session[:device])} login-session-icon' aria-hidden='true'></i>" \
            "<div class='login-session-text'><div class='login-session-device'>#{h(device)}#{badge}</div>" \
            "<div class='login-session-when'>#{h(when_text(session, :today => today))}</div></div>" \
            "#{button}</li>"
    end

    # The list for the profile; with more than limit sessions, the first
    # limit and a button that fetches the rest.
    def self.list_html(sessions, current_sid:, today: Date.today, limit: 10)
        rows = listed(sessions, :current_sid => current_sid)
        shown = limit ? rows.first(limit) : rows
        out = +"<ul class='login-sessions' id='login_sessions'>"
        shown.each { |s| out << row_html(s, :current => s[:sid] == current_sid, :today => today) }
        out << '</ul>'
        if shown.size < rows.size
            out << "<button type='button' class='btn btn-link btn-sm px-0' id='bu_all_sessions'>" \
                "<i class='bi bi-chevron-down'></i>alle #{rows.size} anzeigen</button>"
        end
        out
    end
end
