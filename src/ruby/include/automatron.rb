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

# ------------------- Grammar-based random word sampler (extended) -------------------
module GrammarSampler
  module_function

  # Existing positive sampler (kept as-is)
  def sample(g, count: 20, max_steps: 30, eps_bias: 1.0, seed: nil)
    rng = seed ? Random.new(seed) : Random
    prods = g[:productions]
    start = g[:start]
    productive = RegularGrammar.productive_nonterminals(g)

    words = []
    attempts = 0
    max_attempts = count * 50

    while words.size < count && attempts < max_attempts
      attempts += 1
      output = +''
      nt = start
      steps = 0
      success = false

      while nt && steps < max_steps
        choices = prods[nt]
        break if choices.nil? || choices.empty?

        usable = choices.select { |rhs| rhs == 'ε' || productive.include?(rhs[1..]) }
        if usable.empty?
          if choices.include?('ε')
            usable = ['ε']
          else
            nt = :dead
            break
          end
        end

        total = 0.0
        weighted = usable.map do |rhs|
          w = 1.0
          w += eps_bias if rhs == 'ε'
          total += w
          [rhs, w]
        end

        r = rng.rand * total
        pick = nil
        acc = 0.0
        weighted.each do |rhs, w|
          acc += w
          if r <= acc
            pick = rhs
            break
          end
        end
        pick ||= weighted.last[0]

        if pick == 'ε'
          nt = nil
          success = true
          break
        else
          output << pick[0]
          nt = pick[1..]
        end

        steps += 1
      end

      words << output if success
    end

    words.sort.uniq.sort do |a, b|
      (a.size == b.size) ?
      (a <=> b) :
      (a.size <=> b.size)
    end
  end

  # --- NEW: negatives via DFA random strings that do NOT belong to the language ---
  def sample_negatives_via_dfa(dfa, count: 20, max_len: 20, stop_prob: 0.25, seed: nil)
    rng = seed ? Random.new(seed) : Random
    alphabet = dfa.alphabet.to_a.sort

    negatives = []
    attempts = 0
    max_attempts = count * 200 # allow retries to find enough negatives

    while negatives.size < count && attempts < max_attempts
      attempts += 1
      # random-length string via geometric stop
      s = +''
      len = 0
      while len < max_len
        # stop with probability if we already have at least length 1
        break if len > 0 && rng.rand < stop_prob
        s << alphabet[rng.rand(alphabet.length)]
        len += 1
      end
      s.freeze

      next if s.empty? # avoid empty unless you want it
      next if dfa_accepts?(dfa, s) # we want only rejected strings
      negatives << s
    end

    negatives.sort.uniq.sort do |a, b|
      (a.size == b.size) ?
      (a <=> b) :
      (a.size <=> b.size)
    end
  end

  # --- NEW: convenience mixer ---
  def sample_mixed(dfa, grammar, pos: 20, neg: 20,
                    max_steps: 30, eps_bias: 1.0,
                    max_len: 20, stop_prob: 0.25, seed: nil)
    {
      positives: sample(grammar, count: pos, max_steps: max_steps, eps_bias: eps_bias, seed: seed),
      negatives: sample_negatives_via_dfa(dfa, count: neg, max_len: max_len, stop_prob: stop_prob, seed: seed)
    }
  end

  # --- Helper: simulate DFA acceptance ---
  def dfa_accepts?(dfa, str)
    state = dfa.start
    str.each_char do |ch|
      # if symbol not in alphabet, treat as immediate reject
      return false unless dfa.alphabet.include?(ch)
      state = dfa.transitions[state][ch]
      # if transition missing (shouldn't happen in completed DFA), reject
      return false if state.nil?
    end
    dfa.accepts.include?(state)
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
          "<i class='bi bi-x text-danger'></i>"
        elsif (w = trace[:witnesses][key])
          to = w[:to]
          "<i class='bi bi-x text-danger'></i> <span class='reason'>via #{esc w[:symbol]} → (q_#{states[to[0]]}, q_#{states[to[1]]})</span>"
        else
          "<i class='bi bi-check text-success'></i>"
        end

      # color classes for clarity
      cls =
        if cell.start_with?("<i class='bi bi-x")
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

# ------------------- Regular Grammar from DFA (productive-aware) -------------------
class RegularGrammar
  # Build a right-linear grammar structure from a DFA.
  # Returns:
  # {
  #   nonterminals: Set<String>,
  #   terminals:    Set<String>,
  #   start:        "S",
  #   productions:  { "S" => ["aA", "ε", ...], "A" => ["bB", ...], ... },
  #   state_names:  { dfa_state_id => "S"/"A"/... }   # for reference
  # }
  def self.build(dfa)
    idx_to_name = {}
    letters = ('A'..'Z').to_a
    letters.delete('S')
    ordered = [dfa.start] + (dfa.states.to_a - [dfa.start]).sort
    idx_to_name[dfa.start] = 'S'
    pool = letters.cycle
    (ordered - [dfa.start]).each { |s| idx_to_name[s] = pool.next }

    nonterms = Set.new(idx_to_name.values)
    terms    = Set.new(dfa.alphabet.to_a)
    prods    = Hash.new { |h, k| h[k] = [] }

    dfa.states.each do |s|
      lhs = idx_to_name[s]
      dfa.alphabet.each do |a|
        t = dfa.transitions[s][a]
        prods[lhs] << "#{a}#{idx_to_name[t]}"
      end
      prods[lhs] << 'ε' if dfa.accepts.include?(s)
    end

    { nonterminals: nonterms, terminals: terms, start: 'S', productions: prods, state_names: idx_to_name }
  end

  # Compute productive nonterminals (those that can derive ε).
  # For right-linear grammars built from DFAs, this coincides with DFA states that can reach an accept.
  def self.productive_nonterminals(g)
    prods = g[:productions]
    rev = Hash.new { |h, k| h[k] = Set.new }  # reverse edges: B <- A if A -> aB

    prods.each do |a, rhslist|
      rhslist.each do |rhs|
        next if rhs == 'ε'
        b = rhs[1..]
        rev[b] << a
      end
    end

    productive = Set.new
    queue = []

    # Any A with ε in its productions is productive
    prods.each do |a, rhslist|
      if rhslist.include?('ε')
        productive << a
        queue << a
      end
    end

    # Backward closure: if A -> aB and B productive then A productive
    while (x = queue.shift)
      rev[x].each do |pred|
        next if productive.include?(pred)
        productive << pred
        queue << pred
      end
    end

    productive
  end

  # Pretty-print grammar. By default hides productions that target nonproductive NTs,
  # avoiding confusing rules (e.g., 'a' after 'b' for a?b+) that can never terminate.
  def self.to_text(g, hide_nonproductive: true)
    productive = hide_nonproductive ? productive_nonterminals(g) : nil
    lines = []
    lines << "Nonterminals: #{g[:nonterminals].to_a.join(', ')}"
    lines << "Terminals: #{g[:terminals].to_a.join(', ')}"
    lines << "Start: #{g[:start]}"
    lines << "Productions:"

    g[:productions].keys.sort.each do |lhs|
      rhs_all = g[:productions][lhs].uniq
      rhs = if productive
        rhs_all.select { |r| r == 'ε' || productive.include?(r[1..]) }
      else
        rhs_all
      end
      next if rhs.empty?
      lines << "  #{lhs} → #{rhs.join(' | ')}"
    end
    lines.join("\n")
  end

  # Back-compat helper: what you previously used
  def self.from_dfa(dfa)
    to_text(build(dfa), hide_nonproductive: true)
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
