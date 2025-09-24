#!/usr/bin/env ruby
# frozen_string_literal: true

# Regex → ε-NFA (Thompson) → DFA (subset construction) → Min DFA (Hopcroft) → Right-linear Grammar
# Supported regex features (HS level): symbols a–z, concatenation (implicit), union '|', Kleene star '*', plus '+', optional '?', parentheses '()'.
# Outputs Graphviz DOT files for NFA, DFA, and minimal DFA, and a grammar.txt with a right-linear grammar.
# Usage: ruby regex_to_nfa_dfa_min_grammar.rb "a(b|a)*a+" [output_dir]

require 'set'

# ------------------- Utilities -------------------

# ------------------- Subset Construction trace helper -------------------
module SubsetTrace
  module_function

  # Returns [dfa, trace]
  # trace has:
  #   :alphabet        -> Array of symbols
  #   :start_closure   -> Array of NFA states
  #   :state_map       -> { dfa_state_id => [nfa_state_ids sorted] }
  #   :steps           -> Array of rows:
  #                        { from:[...], symbol:'a', move:[...], closure:[...], to: dfa_state_id or nil }
  def build_with_trace(nfa)
    alphabet = nfa.alphabet.dup

    # ε-closure
    eps = lambda do |set|
      stack = set.to_a
      seen  = Set.new(set)
      while (x = stack.pop)
        (nfa.states[x].trans[:eps] || []).each do |y|
          next if seen.include?(y)
          seen << y
          stack << y
        end
      end
      seen
    end

    # move(S, a)
    move = lambda do |set, sym|
      out = Set.new
      set.each do |s|
        (nfa.states[s].trans[sym] || []).each { |t| out << t }
      end
      out
    end

    idgen = ID.new
    dfa_state_id = {}     # { Set[nfa states] => dfa_id }
    dfa_states   = {}     # { dfa_id => Set[nfa states] }
    transitions  = {}     # { dfa_id => { sym => dfa_id } }
    accepts      = Set.new

    start_set = eps.call(Set[nfa.start])
    start_id  = idgen.next
    dfa_state_id[start_set] = start_id
    dfa_states[start_id]    = start_set

    queue = [start_set]

    trace = {
      alphabet: alphabet.to_a,
      start_closure: start_set.to_a.sort,
      state_map: { start_id => start_set.to_a.sort },
      steps: []
    }

    while (curr = queue.shift)
      sid = dfa_state_id[curr]
      transitions[sid] ||= {}
      accepts << sid if curr.include?(nfa.accept)

      alphabet.each do |sym|
        m = move.call(curr, sym)
        u = eps.call(m)

        row = {
          from: curr.to_a.sort,
          symbol: sym,
          move: m.to_a.sort,
          closure: u.to_a.sort
        }

        if u.empty?
          row[:to] = nil
          trace[:steps] << row
          next
        end

        unless dfa_state_id.key?(u)
          nid = idgen.next
          dfa_state_id[u] = nid
          dfa_states[nid]  = u
          queue << u
          trace[:state_map][nid] = u.to_a.sort
        end

        transitions[sid][sym] = dfa_state_id[u]
        row[:to] = dfa_state_id[u]
        trace[:steps] << row
      end
    end

    # Complete DFA with a dead state if needed
    dead = nil
    dfa_states.keys.each do |sid|
      transitions[sid] ||= {}
      missing = alphabet.count { |sym| !transitions[sid].key?(sym) }
      if missing > 0
        dead ||= idgen.next
        transitions[dead] ||= {}
        alphabet.each { |sym| transitions[dead][sym] = dead }
        dfa_states[dead] = Set.new
        trace[:state_map][dead] = []
      end
      alphabet.each { |sym| transitions[sid][sym] ||= dead }
    end

    dfa = DFA.new(
      states: Set.new(dfa_states.keys),
      start: start_id,
      accepts: accepts,
      alphabet: alphabet,
      transitions: transitions
    )

    [dfa, trace]
  end
end


class ID
  def initialize
    @n = 0
  end
  def next
    x = @n
    @n += 1
    x
  end
end

# ------------------- Regex Parsing -------------------
class RegexParser
  Token = Struct.new(:type, :value) # type: :sym, :op, :lparen, :rparen

  OP_CONCAT = '.' # explicit concatenation operator for internal use
  UNARY = Set['*', '+', '?']

  PRECEDENCE = {
    '|' => 1,
    OP_CONCAT => 2,
    '*' => 3,
    '+' => 3,
    '?' => 3
  }

  ASSOCIATIVITY = {
    '|' => :left,
    OP_CONCAT => :left,
    '*' => :right, # postfix
    '+' => :right,
    '?' => :right
  }

  def initialize(regex)
    @regex = regex.strip
  end

  def tokenize
    tokens = []
    @regex.each_char do |ch|
      case ch
      when '(' then tokens << Token.new(:lparen, ch)
      when ')' then tokens << Token.new(:rparen, ch)
      when '|', '*', '+', '?' then tokens << Token.new(:op, ch)
      else
        if ch =~ /[a-z0-9]/i
          tokens << Token.new(:sym, ch)
        elsif ch =~ /\s/
          next
        else
          raise "Unsupported character in regex: '#{ch}'"
        end
      end
    end
    tokens
  end

  # Insert explicit concatenation operator where needed
  def insert_concat(tokens)
    out = []
    tokens.each_with_index do |tok, i|
      out << tok
      nxt = tokens[i + 1]
      break if nxt.nil?
      if needs_concat?(tok, nxt)
        out << Token.new(:op, OP_CONCAT)
      end
    end
    out
  end

  def needs_concat?(a, b)
    left_types_concat = [:sym, :rparen]
    right_types_concat = [:sym, :lparen]
    return true if left_types_concat.include?(a.type) && right_types_concat.include?(b.type)
    return true if a.type == :sym && b.type == :sym
    return true if a.type == :rparen && b.type == :sym
    return true if a.type == :sym && b.type == :lparen
    return true if a.type == :rparen && b.type == :lparen
    # unary operators are postfix; concatenation needed between (sym|) or unary and (sym|()
    return true if a.type == :op && UNARY.include?(a.value) && (b.type == :sym || b.type == :lparen)
    false
  end

  # Shunting-yard to postfix (RPN)
  def to_postfix
    tokens = insert_concat(tokenize)
    output = []
    stack = []
    tokens.each do |t|
      case t.type
      when :sym
        output << t
      when :op
        if UNARY.include?(t.value)
          # Postfix unary: pop operators with higher precedence only
          while !stack.empty? && stack.last.type == :op && PRECEDENCE[stack.last.value] > PRECEDENCE[t.value]
            output << stack.pop
          end
          stack << t
        else
          while !stack.empty? && stack.last.type == :op && (
            PRECEDENCE[stack.last.value] > PRECEDENCE[t.value] ||
            (PRECEDENCE[stack.last.value] == PRECEDENCE[t.value] && ASSOCIATIVITY[t.value] == :left)
          )
            output << stack.pop
          end
          stack << t
        end
      when :lparen
        stack << t
      when :rparen
        while !stack.empty? && stack.last.type != :lparen
          output << stack.pop
        end
        raise 'Mismatched parentheses' if stack.empty? || stack.last.type != :lparen
        stack.pop
      end
    end
    while !stack.empty?
      raise 'Mismatched parentheses' if stack.last.type == :lparen
      output << stack.pop
    end
    output
  end
end

# ------------------- Thompson ε-NFA -------------------
class NFA
  State = Struct.new(:id, :trans) # trans: {symbol => Set[next_ids], :eps => Set[next_ids]}
  attr_reader :start, :accept, :states, :alphabet

  def initialize(start, accept, states, alphabet)
    @start = start
    @accept = accept
    @states = states
    @alphabet = alphabet # Set of symbols (excludes :eps)
  end

  def self.from_regex(regex)
    parser = RegexParser.new(regex)
    rpn = parser.to_postfix
    idgen = ID.new

    stack = []
    alphabet = Set.new

    rpn.each do |t|
      if t.type == :sym
        s = idgen.next
        e = idgen.next
        states = {}
        states[s] = State.new(s, Hash.new { |h, k| h[k] = Set.new })
        states[e] = State.new(e, Hash.new { |h, k| h[k] = Set.new })
        states[s].trans[t.value] << e
        stack << [s, e, states]
        alphabet << t.value
      elsif t.type == :op
        case t.value
        when RegexParser::OP_CONCAT
          s2, e2, st2 = stack.pop
          s1, e1, st1 = stack.pop
          states = merge_states(st1, st2)
          states[e1].trans[:eps] << s2
          stack << [s1, e2, states]
        when '|'
          s2, e2, st2 = stack.pop
          s1, e1, st1 = stack.pop
          states = merge_states(st1, st2)
          ns = idgen.next
          ne = idgen.next
          states[ns] = State.new(ns, Hash.new { |h, k| h[k] = Set.new })
          states[ne] = State.new(ne, Hash.new { |h, k| h[k] = Set.new })
          states[ns].trans[:eps] << s1
          states[ns].trans[:eps] << s2
          states[e1].trans[:eps] << ne
          states[e2].trans[:eps] << ne
          stack << [ns, ne, states]
        when '*'
          s1, e1, st1 = stack.pop
          states = st1
          ns = idgen.next
          ne = idgen.next
          states[ns] = State.new(ns, Hash.new { |h, k| h[k] = Set.new })
          states[ne] = State.new(ne, Hash.new { |h, k| h[k] = Set.new })
          states[ns].trans[:eps] << s1
          states[ns].trans[:eps] << ne
          states[e1].trans[:eps] << s1
          states[e1].trans[:eps] << ne
          stack << [ns, ne, states]
        when '+'
          s1, e1, st1 = stack.pop
          states = st1
          ns = idgen.next
          ne = idgen.next
          states[ns] = State.new(ns, Hash.new { |h, k| h[k] = Set.new })
          states[ne] = State.new(ne, Hash.new { |h, k| h[k] = Set.new })
          states[ns].trans[:eps] << s1 # must take fragment once
          states[e1].trans[:eps] << s1 # loop
          states[e1].trans[:eps] << ne # or stop
          stack << [ns, ne, states]
        when '?'
          s1, e1, st1 = stack.pop
          states = st1
          ns = idgen.next
          ne = idgen.next
          states[ns] = State.new(ns, Hash.new { |h, k| h[k] = Set.new })
          states[ne] = State.new(ne, Hash.new { |h, k| h[k] = Set.new })
          states[ns].trans[:eps] << s1
          states[ns].trans[:eps] << ne
          states[e1].trans[:eps] << ne
          stack << [ns, ne, states]
        else
          raise "Unknown operator: #{t.value}"
        end
      end
    end

    raise 'Invalid regex expression' unless stack.size == 1
    s, e, st = stack.pop
    new(s, e, st, alphabet)
  end

  def self.merge_states(a, b)
    h = a.dup
    b.each do |k, v|
      if h.key?(k)
        v.trans.each { |sym, to| h[k].trans[sym].merge(to) }
      else
        h[k] = v
      end
    end
    h
  end
end

# ------------------- Subset Construction (ε-closure) -------------------
class DFA
  attr_reader :states, :start, :accepts, :alphabet, :transitions
  # states: Set of state ids (ints)
  # transitions: {state => {symbol => next_state}}

  def initialize(states:, start:, accepts:, alphabet:, transitions:)
    @states = states
    @start = start
    @accepts = accepts
    @alphabet = alphabet
    @transitions = transitions
  end

  def self.from_nfa(nfa)
    alphabet = nfa.alphabet.dup
    # ε-closure helper
    eps = lambda do |set|
      stack = set.to_a
      seen = Set.new(set)
      while (x = stack.pop)
        nfa.states[x].trans[:eps].each do |y|
          next if seen.include?(y)
          seen << y
          stack << y
        end
      end
      seen
    end

    move = lambda do |set, sym|
      nxt = Set.new
      set.each do |s|
        nfa.states[s].trans[sym].each { |t| nxt << t }
      end
      nxt
    end

    idgen = ID.new
    dfa_state_id = {}
    dfa_states = {}

    start_set = eps.call(Set[nfa.start])
    start_id = idgen.next
    dfa_state_id[start_set] = start_id
    dfa_states[start_id] = start_set

    queue = [start_set]
    transitions = {}
    accepts = Set.new

    while (curr = queue.shift)
      sid = dfa_state_id[curr]
      transitions[sid] ||= {}
      accepts << sid if curr.include?(nfa.accept)
      alphabet.each do |sym|
        u = eps.call(move.call(curr, sym))
        if u.empty?
          # handled later when completing DFA
          next
        end
        unless dfa_state_id.key?(u)
          nid = idgen.next
          dfa_state_id[u] = nid
          dfa_states[nid] = u
          queue << u
        end
        transitions[sid][sym] = dfa_state_id[u]
      end
    end

    # Complete DFA by adding dead state if needed
    dead = nil
    dfa_states.keys.each do |sid|
      transitions[sid] ||= {}
      n_missing = alphabet.count { |sym| !transitions[sid].key?(sym) }
      if n_missing > 0
        dead ||= idgen.next
        transitions[dead] ||= {}
        alphabet.each { |sym| transitions[dead][sym] = dead }
        dfa_states[dead] = Set.new # empty set
        n_missing.times {} # noop
      end
      alphabet.each do |sym|
        transitions[sid][sym] ||= dead
      end
    end

    states = Set.new(dfa_states.keys)
    DFA.new(states: states, start: start_id, accepts: accepts, alphabet: alphabet, transitions: transitions)
  end
end

# ------------------- Hopcroft Minimization -------------------

class DFAMinimizer
  # Classic table-filling (distinguishability) minimization with trace.
  def self.minimize_with_trace(dfa)
    states = dfa.states.to_a.sort
    index = {}
    states.each_with_index { |s, i| index[s] = i }
    n = states.size
    alphabet = dfa.alphabet.to_a

    marked = Hash.new(false)
    pair = lambda do |i, j|
      i, j = [i, j].min, [i, j].max
      [i, j]
    end

    accepting = dfa.accepts
    initial_marked = []
    (0...n).each do |i|
      (i+1...n).each do |j|
        si = states[i]
        sj = states[j]
        if accepting.include?(si) ^ accepting.include?(sj)
          marked[pair.call(i, j)] = true
          initial_marked << [i, j]
        end
      end
    end

    witnesses = {} # { [i,j] => {symbol: c, to: [ii,jj]} }
    changed = true
    while changed
      changed = false
      (0...n).each do |i|
        (i+1...n).each do |j|
          next if marked[pair.call(i, j)]
          si = states[i]
          sj = states[j]
          alphabet.each do |a|
            ti = dfa.transitions[si][a]
            tj = dfa.transitions[sj][a]
            ii = index[ti]
            jj = index[tj]
            if marked[pair.call(ii, jj)]
              marked[pair.call(i, j)] = true
              witnesses[pair.call(i,j)] = { symbol: a, to: pair.call(ii, jj) }
              changed = true
              break
            end
          end
        end
      end
    end

    parent = Array.new(n) { |i| i }
    rank = Array.new(n, 0)
    find = lambda do |x|
      while parent[x] != x
        parent[x] = parent[parent[x]]
        x = parent[x]
      end
      x
    end
    unite = lambda do |x, y|
      rx = find.call(x)
      ry = find.call(y)
      return if rx == ry
      if rank[rx] < rank[ry]
        parent[rx] = ry
      elsif rank[rx] > rank[ry]
        parent[ry] = rx
      else
        parent[ry] = rx
        rank[rx] += 1
      end
    end

    (0...n).each do |i|
      (i+1...n).each do |j|
        unless marked[pair.call(i, j)]
          unite.call(i, j)
        end
      end
    end

    rep_to_block = {}
    block_id = {}
    next_block = 0
    (0...n).each do |i|
      r = find.call(i)
      rep_to_block[r] ||= begin
        bid = next_block
        next_block += 1
        bid
      end
      block_id[states[i]] = rep_to_block[r]
    end

    new_states = Set.new((0...next_block).to_a)
    new_start = block_id[dfa.start]
    new_accepts = Set.new
    dfa.accepts.each { |s| new_accepts << block_id[s] }

    new_trans = {}
    new_states.each do |b|
      new_trans[b] ||= {}
    end

    reps = {}
    block_id.each do |old, b|
      reps[b] ||= old
    end
    new_states.each do |b|
      rep = reps[b]
      alphabet.each do |a|
        to_old = dfa.transitions[rep][a]
        new_trans[b][a] = block_id[to_old]
      end
    end

    dmin = DFA.new(states: new_states, start: new_start, accepts: new_accepts, alphabet: dfa.alphabet, transitions: new_trans)

    # Build trace object
    trace = {
      states: states,
      accepts: states.map { |s| dfa.accepts.include?(s) },
      initial_marked: initial_marked,
      witnesses: witnesses, # keys are pairs [i,j]
      blocks: (0...next_block).map { |b| states.select { |s| block_id[s] == b } },
      block_map: block_id
    }

    [dmin, trace]
  end

  # Backward-compatible call
  def self.minimize(dfa)
    dmin, _ = minimize_with_trace(dfa)
    dmin
  end
end

# ------------------- Graphviz Rendering -------------------
module DotRender
  module_function

  # Plain DOT node id (not displayed)
  def node_id(id)
    "q_#{id}"
  end

  # HTML-like label: q with subscript 0..9,A..Z then fallback to numeric
  def node_label(id)
    base = ('0'..'9').to_a + ('A'..'Z').to_a
    sym = id < base.size ? base[id] : id.to_s
    # Subscript with smaller font size
    "<q<SUB><FONT POINT-SIZE=\"10\">#{sym}</FONT></SUB>>"
  end

  def sink_id
    'q_F'
  end
  def sink_label
    '<q<SUB><FONT POINT-SIZE="10">F</FONT></SUB>>'
  end

  def nfa(nfa)
    lines = []
    lines << 'digraph NFA {'
    lines << '  rankdir=LR;'
    lines << '  node [shape=circle];'
    lines << '  __start [shape=point,label=""];'
    lines << "  __start -> #{node_id(nfa.start)};"

    # Nodes
    nfa.states.each do |id, st|
      shape = (id == nfa.accept) ? 'doublecircle' : 'circle'
      lines << "  #{node_id(id)} [shape=#{shape},label=#{node_label(id)}];"
    end

    # Grouped edges per (from,to)
    nfa.states.each do |id, st|
      grouped = Hash.new { |h, k| h[k] = [] }
      st.trans.each do |sym, toset|
        toset.each do |to|
          grouped[to] << (sym == :eps ? 'ε' : sym)
        end
      end
      grouped.each do |to, syms|
        syms = syms.sort
        lines << "  #{node_id(id)} -> #{node_id(to)} [label=\"#{syms.join(',')}\"];"
      end
    end

    lines << '}'
    lines.join("
")
  end

  # detect canonical sink (dead) state: non-accepting and δ(s,a)=s for all a
  def detect_sink(dfa)
    dfa.states.each do |s|
      next if dfa.accepts.include?(s)
      trans = dfa.transitions[s] || {}
      return s if dfa.alphabet.all? { |a| trans[a] == s }
    end
    nil
  end

  # Render DFA; when sink_explicit=false, hide sink node and edges to it.
  def dfa(dfa, title: 'DFA', sink_explicit: true, sink_label_text: 'q_F')
    sink = detect_sink(dfa)
    lines = []
    lines << "digraph #{title} {"
    lines << '  rankdir=LR;'
    lines << '  node [shape=circle];'
    lines << '  __start [shape=point,label=""];'
    lines << "  __start -> #{node_id(dfa.start)};"

    # Nodes
    dfa.states.each do |s|
      next if !sink_explicit && s == sink
      shape = dfa.accepts.include?(s) ? 'doublecircle' : 'circle'
      nid = (sink_explicit && s == sink) ? sink_id : node_id(s)
      lab = (sink_explicit && s == sink) ? sink_label : node_label(s)
      lines << "  #{nid} [shape=#{shape},label=#{lab}];"
    end

    # Grouped edges per (from,to)
    dfa.states.each do |s|
      next if !sink_explicit && s == sink
      grouped = Hash.new { |h, k| h[k] = [] }
      dfa.alphabet.each do |c|
        t = dfa.transitions[s][c]
        next if !sink_explicit && t == sink
        from = (sink_explicit && s == sink) ? sink_id : node_id(s)
        to   = (sink_explicit && t == sink) ? sink_id : node_id(t)
        grouped[[from, to]] << c
      end
      grouped.each do |(from, to), syms|
        syms = syms.sort
        lines << "  #{from} -> #{to} [label=\"#{syms.join(',')}\"];"
      end
    end

    if !sink_explicit && sink
      lines << '  subgraph cluster_legend {'
      lines << '    label="Legend"; style=dashed;'
      lines << '    l1 [shape=note,label="Missing transitions go to implicit sink q_F."];'
      lines << '  }'
    end

    lines << '}'
    lines.join("
")
  end
end

# ------------------- HTML Notes -------------------
module HtmlNotes
  module_function

  def esc(s)
    CGI.escapeHTML(s.to_s)
  end

  def subset(trace)
    alpha = trace[:alphabet]
    rows = trace[:steps].map do |r|
      "<tr><td>{#{r[:from].join(', ')}}</td><td>#{esc r[:symbol]}</td><td>{#{r[:move].join(', ')}}</td><td>ε-closure → {#{r[:closure].join(', ')}}</td><td>#{r[:to] ? "q_#{r[:to]}" : '—'}</td></tr>"
    end.join

    map_rows = trace[:state_map].sort_by { |k,_| k }.map do |id, set|
      "<tr><td>q_#{id}</td><td>{#{set.join(', ')}}</td></tr>"
    end.join

    <<~HTML
    <section>
      <h2>Subset Construction (ε-closures)</h2>
      <p><strong>Alphabet:</strong> #{alpha.join(', ')}</p>
      <p><strong>Start ε-closure:</strong> {#{trace[:start_closure].join(', ')}}</p>
      <h3>DFA State Map (subset → state)</h3>
      <table border="1" cellpadding="4" cellspacing="0">
        <tr><th>DFA state</th><th>NFA subset</th></tr>
        #{map_rows}
      </table>
      <h3>Transition Derivations</h3>
      <table border="1" cellpadding="4" cellspacing="0">
        <tr><th>From subset</th><th>Symbol</th><th>move()</th><th>ε-closure</th><th>To DFA</th></tr>
        #{rows}
      </table>
    </section>
    HTML
  end

def table_minimization(trace)
  states  = trace[:states]
  accepts = trace[:accepts]
  n = states.size

  head = (1...n).map { |j| "<th>q_#{states[j]}</th>" }.join

  body = (0...n-1).map do |i|
    # pad left with i cells so the upper triangle aligns under the headers
    pad = i.times.map { '<td class="na">—</td>' }.join

    row_cells = (i+1...n).map do |j|
      key  = [i, j]
      cell =
        if trace[:initial_marked].include?(key)
          '× <span class="reason">(acc/non-acc)</span>'
        elsif (w = trace[:witnesses][key])
          to = w[:to]
          "× <span class=\"reason\">via #{esc w[:symbol]} → (q_#{states[to[0]]}, q_#{states[to[1]]})</span>"
        else
          '✓ <span class="reason">(equiv)</span>'
        end

      # color classes for clarity
      cls =
        if cell.start_with?('×')
          'marked'
        else
          'equiv'
        end

      "<td class=\"#{cls}\">#{cell}</td>"
    end.join

    "<tr><th>q_#{states[i]}</th>#{pad}#{row_cells}</tr>"
  end.join

  css = <<~CSS
    table.min-table{border-collapse:collapse;margin:12px 0}
    table.min-table th,table.min-table td{padding:6px 8px;border:1px solid #999;text-align:center}
    table.min-table td.na{background:#f7f7f7;color:#bbb;font-style:italic}
    table.min-table td.marked{color:#b00020}
    table.min-table td.equiv{color:#006400}
    table.min-table .reason{color:#444;font-weight:normal}
  CSS

  parts = trace[:blocks].map.with_index { |blk, i| "B#{i} = { #{blk.map { |s| "q_#{s}" }.join(', ')} }" }.join('<br>')

  <<~HTML
  <section>
    <h2>Minimization (Table-Filling Method)</h2>
    <p><strong>Accepting states:</strong> #{states.each_with_index.map { |s, i| accepts[i] ? "q_#{s}" : nil }.compact.join(', ')}</p>
    <h3>Distinguishability Table (upper triangle)</h3>
    <style>#{css}</style>
    <table class="min-table">
      <tr><th></th>#{head}</tr>
      #{body}
    </table>
    <h3>Final Partitions</h3>
    <p>#{parts}</p>
  </section>
  HTML
end


  def wrap(*sections)
    <<~HTML
    <!doctype html>
    <html>
    <head>
      <meta charset="utf-8" />
      <title>Automata Notes</title>
      <style>
        body{font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Arial,sans-serif;line-height:1.45;padding:16px;max-width:960px;margin:auto}
        table{border-collapse:collapse;margin:12px 0}
        th,td{padding:6px 8px}
        h2{margin-top:28px}
        code,kbd{background:#f5f5f5;padding:1px 4px;border-radius:4px}
      </style>
    </head>
    <body>
      #{sections.join("
")}
    </body>
    </html>
    HTML
  end
end

# ------------------- Regular Grammar from DFA -------------------
class RegularGrammar
  # Right-linear grammar from DFA: For each state A and transition on a to B, add A → aB. For accepting states, add A → ε.
  def self.from_dfa(dfa)
    # Name nonterminals: S for start, then A, B, C...
    names = {}
    ordered = [dfa.start] + (dfa.states.to_a - [dfa.start]).sort
    idx_to_name = {}
    letters = ('A'..'Z').to_a
    letters.delete('S')
    idx_to_name[dfa.start] = 'S'
    pool = letters.cycle
    (ordered - [dfa.start]).each do |s|
      idx_to_name[s] = pool.next
    end

    productions = Hash.new { |h, k| h[k] = [] }

    dfa.states.each do |s|
      lhs = idx_to_name[s]
      dfa.alphabet.each do |a|
        t = dfa.transitions[s][a]
        productions[lhs] << "#{a}#{idx_to_name[t]}"
      end
      productions[lhs] << 'ε' if dfa.accepts.include?(s)
    end

    # Serialize
    lines = []
    lines << "Nonterminals: #{idx_to_name.values.uniq.join(', ')}"
    lines << "Terminals: #{dfa.alphabet.to_a.join(', ')}"
    lines << "Start: S"
    lines << "Productions:"
    productions.keys.sort.each do |lhs|
      rhs = productions[lhs].uniq
      lines << "  #{lhs} → #{rhs.join(' | ')}"
    end
    lines.join("\n")
  end
end

# ------------------- Main -------------------
if __FILE__ == $0
  require 'cgi'
  regex = ARGV[0] || (abort "Usage: ruby #{File.basename(__FILE__)} \"regex\" [output_dir]")
  outdir = ARGV[1] || 'out'
  Dir.mkdir(outdir) unless Dir.exist?(outdir)

  puts "Parsing regex: #{regex}"
  nfa = NFA.from_regex(regex)
  File.write(File.join(outdir, 'nfa.dot'), DotRender.nfa(nfa))
  puts "Wrote #{outdir}/nfa.dot"
  system("dot -Tsvg #{outdir}/nfa.dot -o #{outdir}/nfa.svg")

  # DFA with trace (subset construction)
  dfa, subset_trace = SubsetTrace.build_with_trace(nfa)
  File.write(File.join(outdir, 'dfa.dot'), DotRender.dfa(dfa, title: 'DFA', sink_explicit: true))
  puts "Wrote #{outdir}/dfa.dot"
  system("dot -Tsvg #{outdir}/dfa.dot -o #{outdir}/dfa.svg")
  File.write(File.join(outdir, 'dfa_implicit_sink.dot'), DotRender.dfa(dfa, title: 'DFA_ImplicitSink', sink_explicit: false))
  system("dot -Tsvg #{outdir}/dfa_implicit_sink.dot -o #{outdir}/dfa_implicit_sink.svg")

  # Minimization with trace (table method)
  min, min_trace = DFAMinimizer.minimize_with_trace(dfa)
  File.write(File.join(outdir, 'min_dfa.dot'), DotRender.dfa(min, title: 'MinDFA', sink_explicit: true))
  puts "Wrote #{outdir}/min_dfa.dot"
  system("dot -Tsvg #{outdir}/min_dfa.dot -o #{outdir}/min_dfa.svg")
  File.write(File.join(outdir, 'min_dfa_implicit_sink.dot'), DotRender.dfa(min, title: 'MinDFA_ImplicitSink', sink_explicit: false))
  system("dot -Tsvg #{outdir}/min_dfa_implicit_sink.dot -o #{outdir}/min_dfa_implicit_sink.svg")

  grammar = RegularGrammar.from_dfa(min)
  File.write(File.join(outdir, 'grammar.txt'), grammar)
  puts "Wrote #{outdir}/grammar.txt"

  # Write HTML notes
  notes_html = HtmlNotes.wrap(
    HtmlNotes.subset(subset_trace),
    HtmlNotes.table_minimization(min_trace)
  )
  File.write(File.join(outdir, 'notes.html'), notes_html)
  puts "Wrote #{outdir}/notes.html"
end
