#!/usr/bin/env ruby
# frozen_string_literal: true

# Regex → ε-NFA (Thompson) → DFA (subset construction) → Min DFA (Hopcroft) → Right-linear Grammar
# Supported regex features (HS level): symbols a–z, concatenation (implicit), union '|', Kleene star '*', plus '+', optional '?', parentheses '()'.
# Outputs Graphviz DOT files for NFA, DFA, and minimal DFA, and a grammar.txt with a right-linear grammar.
# Usage: ruby regex_to_nfa_dfa_min_grammar.rb "a(b|a)*a+" [output_dir]

require 'set'

INCLUDE_SINK = false  # set true to include the sink state in the table

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

    eps_closures = {}
    nfa.states.each_key do |q|
      eps_closures[q] = eps.call(Set[q]).to_a.sort
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
      eps_closures: eps_closures,
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

  # Collapse linear ε-connectors:
  # u --ε--> v  where
  #   (i)  u has no outgoing edges except that ε to v
  #   (ii) v has no incoming edges except that same ε from u
  # We merge v into u and repeat to fixpoint.
  def collapse_simple_eps!
    loop do
      # Build incoming maps (both total and ε-only)
      eps_in  = Hash.new { |h, k| h[k] = Set.new }
      in_any  = Hash.new { |h, k| h[k] = Set.new }

      @states.each do |from, st|
        st.trans.each do |sym, toset|
          toset.each do |to|
            in_any[to] << from
            eps_in[to]  << from if sym == :eps
          end
        end
      end

      changed = false

      @states.each do |u, su|
        eps_out = (su.trans[:eps] || Set.new).dup
        next unless eps_out.size == 1
        v = eps_out.first

        # condition (i): u has exactly that one ε and no other outs
        only_that_eps =
          su.trans.all? do |sym, toset|
            if sym == :eps
              toset.size == 1 && toset.include?(v)
            else
              toset.empty?
            end
          end
        next unless only_that_eps

        # condition (ii): v has no incoming edges except u via ε
        next unless in_any[v].size == 1 && in_any[v].include?(u)
        next unless eps_in[v].size == 1 && eps_in[v].include?(u)

        # Merge v into u:
        sv = @states[v]

        # 1) remove u -> v ε
        su.trans[:eps].delete(v)

        # 2) move all v's outgoing edges to u
        sv.trans.each do |sym, toset|
          next if sym == :eps && toset.include?(u) # avoid immediate u-ε->u self-loop
          toset.each { |w| su.trans[sym] << w }
        end

        # 3) redirect all edges that pointed to v, to point to u (should only be u via ε,
        #     but this keeps us safe if something slipped through)
        @states.each do |p, sp|
          sp.trans.each do |sym, toset|
            if toset.delete?(v)
              sp.trans[sym] << u
            end
          end
        end

        # 4) adjust start/accept if needed
        if @start == v
          @start = u
        end
        if @accept == v
          @accept = u
        end

        # 5) delete v
        @states.delete(v)

        changed = true
        break
      end

      break unless changed
    end
  end  

  def self.from_regex(regex, collapse_simple_eps: true)
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
    nfa = new(s, e, st, alphabet)

    nfa.collapse_simple_eps! if collapse_simple_eps
    nfa
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

  def self._detect_sink(dfa)
    dfa.states.find do |s|
      !dfa.accepts.include?(s) && dfa.alphabet.all? { |a| dfa.transitions[s][a] == s }
    end
  end

  def self.minimize_with_trace(dfa)
    sink = _detect_sink(dfa) 
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
      block_map: block_id,
      sink: sink
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
  def node_label(id, q = 'q')
    # base = ('0'..'9').to_a + ('A'..'Z').to_a
    # sym = id < base.size ? base[id] : id.to_s
    # Subscript with smaller font size
    "<#{q}<SUB><FONT POINT-SIZE=\"10\">#{id}</FONT></SUB>>"
  end

  def sink_id
    'q_F'
  end
  def sink_label
    '<q<SUB><FONT POINT-SIZE="10">F</FONT></SUB>>'
  end

  def nfa(nfa, q: 'q')
    lines = []
    lines << 'digraph NFA {'
    lines << '  rankdir=LR;'
    lines << "graph [fontsize = 14, nodesep = 0.2, ranksep = 0.3];"
    lines << "node [fontsize = 14, shape = circle, margin = 0, style = filled, fillcolor=\"#f0f0f0\"];"
    lines << "edge [fontsize = 14, arrowsize = 0.6, color = \"#000000\"];"
    lines << 'splines=true;'

    lines << '  __start [shape=point, color="#ffffff", label=""];'
    lines << "  __start -> #{node_id(nfa.start)};"

    # Nodes
    nfa.states.each do |id, st|
      shape = (id == nfa.accept) ? 'doublecircle' : 'circle'
      lines << "  #{node_id(id)} [shape=#{shape},label=#{node_label(id, q)}];"
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
  def dfa(dfa, title: 'DFA', sink_explicit: true, sink_label_text: 'q_F', q: 'q')
    sink = detect_sink(dfa)
    lines = []
    lines << "digraph #{title} {"
    lines << '  rankdir=LR;'
    lines << "graph [fontsize = 14, nodesep = 0.2, ranksep = 0.3];"
    lines << "node [fontsize = 14, shape = circle, margin = 0, style = filled, fillcolor=\"#f0f0f0\"];"
    lines << "edge [fontsize = 14, arrowsize = 0.6, color = \"#000000\"];"
    lines << '  __start [shape=point, color="#ffffff", label=""];'
    lines << "  __start -> #{node_id(dfa.start)};"

    # Nodes
    dfa.states.each do |s|
      next if !sink_explicit && s == sink
      shape = dfa.accepts.include?(s) ? 'doublecircle' : 'circle'
      nid = (sink_explicit && s == sink) ? sink_id : node_id(s)
      lab = (sink_explicit && s == sink) ? sink_label : node_label(s, q)
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

    # if !sink_explicit && sink
    #   lines << '  subgraph cluster_legend {'
    #   lines << '    label=""; style=invisible;'
    #   lines << '    l1 [shape=note,label="Fehlende Übergänge\nführen in einen\nimpliziten Fehlerzustand."];'
    #   lines << '  }'
    # end

    lines << '}'
    lines.join("\n")
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

    # Map subset -> dfa id (string key: "0,2,4")
    subset_key = ->(arr) { arr.join(',') }
    subset_to_dfa = {}
    trace[:state_map].each do |dfa_id, set|
        subset_to_dfa[subset_key.call(set)] = dfa_id
    end

    # Group steps by from-subset; for each from, map symbol -> step row
    by_from = Hash.new { |h, k| h[k] = {} }
    trace[:steps].each do |r|
        key = subset_key.call(r[:from])
        by_from[key][r[:symbol]] = r
    end

    # Sort rows by DFA id if we have it; otherwise by subset lexicographically
    ordered_from_keys = by_from.keys.sort_by do |k|
        subset_to_dfa.key?(k) ? [0, subset_to_dfa[k]] : [1, k]
    end

    # Build header: one column per symbol
    head = "<tr><th>Zustand (DEA)</th><th>Zustände (NEA)</th>" +
            alpha.map { |a| "<th>#{CGI.escapeHTML(a)}</th>" }.join +
            "</tr>"

    # Pretty subset like "{0, 2, 4}"
    fmt_set = ->(arr) { '{' + arr.map { |x| 'q<sub>' + x.to_s + '</sub>' }.join('; ') + '}' }

    # Build rows
    body = ordered_from_keys.map do |k|
        # Display DFA name if known
        dfa_tag = subset_to_dfa[k] ? "r<sub>#{subset_to_dfa[k]}</sub>" : ''

        cells = alpha.map do |sym|
            r = by_from[k][sym]
            if r.nil?
                '<td>—</td>'
            elsif r[:closure].empty?
                '<td>∅</td>'
            else
                #"<td>#{fmt_set.call(r[:closure])}</td>"
                "<td>r<sub>#{subset_to_dfa[r[:closure].join(',')]}</sub></td>"
            end
        end.join

        "<tr><td>#{dfa_tag}</td><td>{#{k.split(',').map { |x| 'q<sub>' + x + '</sub>'}.join('; ')}}</td>#{cells}</tr>"
    end.join

    # ε-closures table (kept, just above)
    closures_rows = trace[:eps_closures].sort_by { |q, _| q }.map do |q, set|
        "<tr><td>q<sub>#{q}</sub></td><td>{#{set.map { |x| 'q<sub>' + x.to_s + '</sub>' }.join('; ')}}</td></tr>"
    end.join

    # (optional) keep the state map; comment out if you don’t want it
    map_rows = trace[:state_map].sort_by { |k,_| k }.map do |id, set|
        "<tr><td>q_#{id}</td><td>{#{set.join(', ')}}</td></tr>"
    end.join

    <<~HTML
    <section>
        <h4>ε-Hüllen aller Zustände</h4>
        <div style="max-width: 100%; overflow-x: auto;">
        <table class='table' style='width: unset;'>
        <tr><th>q</th><th>E(q)</th></tr>
        #{closures_rows}
        </table>

        <p><strong>Start ε-closure:</strong> {#{trace[:start_closure].join(', ')}}</p>

        <!--
        <h3>DFA State Map (subset → state)</h3>
        <table class='table' style='width: unset;'>
        <tr><th>DFA state</th><th>NFA subset</th></tr>
        #{map_rows}
        </table>
        -->

        <h4>Übergänge</h4>
        <div style="max-width: 100%; overflow-x: auto;">
        <table class='table' style='width: unset;'>
        #{head}
        #{body}
        </table>
        </div>
        </div>
    </section>
    HTML
  end

  def table_minimization(trace)
    # --- config ---
    states  = trace[:states]
    accepts = trace[:accepts]
    sink    = trace[:sink] rescue nil
  
    # which states to render
    vis_states = (INCLUDE_SINK || sink.nil?) ? states : states.reject { |s| s == sink }
  
    # map original state -> original index
    orig_idx = {}
    states.each_with_index { |s, i| orig_idx[s] = i }
  
    # header (all columns)
    head = "<tr><th></th>" +
           vis_states.map { |s| "<th>r<sub>#{s}</sub></th>" }.join +
           "</tr>"
  
    # helper to read status for pair (i,j) using original indices, regardless of order
    cell_for = lambda do |i_vis, j_vis|
      i0 = orig_idx[vis_states[i_vis]]
      j0 = orig_idx[vis_states[j_vis]]
      i, j = [i0, j0].min, [i0, j0].max
      key = [i, j]
  
      if i_vis == j_vis
        ['—', 'diag'] # diagonal
      elsif j_vis < i_vis
        ['—', 'na']   # lower triangle (not used)
      elsif trace[:initial_marked].include?(key)
        ["<i class='bi bi-x text-danger'></i>", 'marked']
      elsif (w = trace[:witnesses][key])
        to_i, to_j = w[:to]
        [
          "<i class='bi bi-x text-danger'></i> "\
          "<span class='reason'>(#{CGI.escapeHTML(w[:symbol])})</span>",
          'marked'
        ]
      else
        ["<i class='bi bi-check text-success'></i>", 'equiv']
      end
    end
  
    # build body: one row per visible state, one cell per visible state
    body = vis_states.each_index.map do |i|
      row_cells = vis_states.each_index.map do |j|
        html, cls = cell_for.call(i, j)
        "<td class=\"#{cls}\">#{html}</td>"
      end.join
      "<tr><th>r<sub>#{vis_states[i]}</sub></th>#{row_cells}</tr>"
    end.join
  
    css = <<~CSS
      table.min-table{border-collapse:collapse;margin:12px 0}
      table.min-table th,table.min-table td{padding:6px 8px;border:1px solid #999;text-align:center;vertical-align: top;}
      table.min-table td.na{background:#f7f7f7;color:#bbb;font-style:italic}
      table.min-table td.diag{background:#f0f0f0;color:#999}
      table.min-table td.marked{color:#b00020}
      table.min-table td.equiv{color:#006400}
      table.min-table .reason{color:#444;font-weight:normal;}
    CSS
  
    sink = trace[:sink] rescue nil
    visible_blocks = sink ? trace[:blocks].reject { |blk| blk.include?(sink) } : trace[:blocks]
    parts = visible_blocks.map.with_index do |blk, i|
        "<td>s<sub>#{i}</sub></td><td>{#{blk.map do |s|
            "r<sub>#{s}</sub>"
        end.join('; ')}}</td>"
    end.map do |x|
        "<tr>#{x}</tr>"
    end.join
  
    <<~HTML
    <section>
      <style>#{css}</style>
      <div style="max-width: 100%; overflow-x: auto;">
      <table class="min-table">
        #{head}
        #{body}
      </table>
      </div>
      <h4>Resultierende Partitionen</h4>
      <div style="max-width: 100%; overflow-x: auto;">
      <table class='table' style='width: unset;'>
      <tr><th>Zustand (min. DEA)</th><th>Partition</th></tr>
      #{parts}
      </table>
      </div>
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
  def self.to_html(g, hide_nonproductive: true)
    productive = hide_nonproductive ? productive_nonterminals(g) : nil
    lines = []
    lines << "Nicht-Terminale: #{g[:nonterminals].to_a.join(', ')}<br>"
    lines << "Terminale: #{g[:terminals].to_a.join(', ')}<br>"
    lines << "Start: #{g[:start]}<br>"
    lines << "Produktionsregeln:<br>"
    lines << "<ul>"


    # --- S first, then others alphabetically ---
    ordered_lhs = ['S'] + (g[:productions].keys - ['S']).sort

    ordered_lhs.each do |lhs|
        next unless g[:productions].key?(lhs)
        rhs_all = g[:productions][lhs].uniq
        rhs = if productive
        rhs_all.select { |r| r == 'ε' || productive.include?(r[1..]) }
        else
        rhs_all
        end
        next if rhs.empty?
        lines << "<li>#{lhs} → #{rhs.join(' | ')}</li>"
    end

    lines << "</ul>"
    lines.join("\n")
  end


  # Back-compat helper: what you previously used
  def self.from_dfa(dfa)
    to_html(build(dfa), hide_nonproductive: true)
  end

# Convert "aB" / "ε" to pairs [w, Y] where w is a terminal string, Y is a nonterminal or nil
  def self._expand_pairs(g)
    prods2 = Hash.new { |h, k| h[k] = [] }
    g[:productions].each do |lhs, rhslist|
      rhslist.each do |rhs|
        if rhs == 'ε'
          prods2[lhs] << ['', nil]
        else
          # Our construction produces exactly 1 terminal followed by 1 nonterminal.
          # But the simplifier will create longer w's; we keep it general.
          w = rhs[0]
          y = rhs[1..]
          prods2[lhs] << [w, y]
        end
      end
    end
    prods2
  end

  # Turn pairs back to strings "wY" / "ε"
  def self._compact_pairs_to_strings(prods2)
    out = Hash.new { |h, k| h[k] = [] }
    prods2.each do |lhs, arr|
      arr.each do |w, y|
        out[lhs] << (y ? "#{w}#{y}" : (w.empty? ? 'ε' : w))
      end
    end
    out
  end

  # Recompute how often each nonterminal appears as the trailing NT on RHS
  def self._rhs_refcounts(prods2)
    rc = Hash.new(0)
    prods2.each_value do |arr|
      arr.each do |w, y|
        rc[y] += 1 if y
      end
    end
    rc
  end

  # Simplify by (i) removing non-productive targets, then (ii) inlining linear chains.
  # Returns a new grammar hash like `build`, but RHS may contain w∈Σ* (multi-char).
  def self.simplify(g)
    start = g[:start]
    prods2 = _expand_pairs(g)  # { "A" => [[w, Y], ...] }

    # (i) Drop any RHS whose trailing NT cannot reach ε
    productive = productive_nonterminals(g)
    prods2.each do |lhs, list|
      list.select! { |w, y| y.nil? || productive.include?(y) } # keep ε (y=nil) and productive targets
    end

    # Remove LHS that lost all productions (except S if it has ε)
    prods2.keys.each do |lhs|
      if prods2[lhs].empty?
        prods2.delete(lhs) unless lhs == start
      end
    end

    # (ii) Inline ε, w, wY chains until fixed point
    changed = true
    while changed
      changed = false

      # Inline X -> ε
      prods2.keys.each do |x|
        arr = prods2[x]
        next unless arr.size == 1 && arr[0] == ['', nil]
        prods2.each_value do |list|
          list.each_index do |i|
            w, y = list[i]
            if y == x
              list[i] = [w, nil]      # drop the trailing NT
              changed = true
            end
          end
        end
        prods2.delete(x) unless x == start
      end

      # Inline X -> w (terminal-only)
      prods2.keys.each do |x|
        next if x == start
        arr = prods2[x]
        next unless arr.size == 1
        w1, y1 = arr[0]
        next unless y1.nil? && !w1.empty?

        used = false
        prods2.each_value do |list|
          list.each_index do |i|
            w, y = list[i]
            if y == x
              list[i] = [w + w1, nil]
              used = true
              changed = true
            end
          end
        end
        prods2.delete(x) if used
      end

      # Inline X -> wY (chain)
      prods2.keys.each do |x|
        next if x == start
        arr = prods2[x]
        next unless arr.size == 1
        w1, y1 = arr[0]
        next if y1.nil?

        used = false
        prods2.each_value do |list|
          list.each_index do |i|
            w, y = list[i]
            if y == x
              list[i] = [w + w1, y1]
              used = true
              changed = true
            end
          end
        end
        prods2.delete(x) if used
      end
    end

    # Rebuild grammar object
    g2 = {
      nonterminals: g[:nonterminals].dup,
      terminals:    g[:terminals].dup,
      start:        start,
      productions:  _compact_pairs_to_strings(prods2),
      state_names:  g[:state_names]
    }

    # Keep only actually-used nonterminals (LHS plus any trailing NTs on RHS)
    still = Set.new(g2[:productions].keys)
    g2[:productions].each_value do |list|
      list.each do |rhs|
        next if rhs == 'ε'
        if rhs =~ /[A-Z]+$/  # trailing NT
          still << $&
        end
      end
    end
    g2[:nonterminals] = still
    g2
  end

  # Start-first pretty printer for compact grammar
  def self.to_html_compact(g)
    keys = g[:productions].keys
    ordered = ['S'] + (keys - ['S']).sort
    lines = []
    lines << "Nicht-Terminale: #{g[:nonterminals].to_a.sort.join(', ')}<br>"
    lines << "Terminale: #{g[:terminals].to_a.sort.join(', ')}<br>"
    lines << "Start: #{g[:start]}<br>"
    lines << "Produktionsregeln:<br>"
    lines << "<ul>"
    ordered.each do |lhs|
      next unless g[:productions].key?(lhs)
      rhs = g[:productions][lhs].uniq
      # optional nicety: sort short (terminal-only) before chain rules
      rhs.sort_by! { |r| r == 'ε' ? -1 : (r =~ /[A-Z]+$/ ? 1 : 0) }
      lines << "<li>#{lhs} → #{rhs.join(' | ')}</li>"
    end
    lines << "</ul>"
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
  File.write(File.join(outdir, 'dfa.dot'), DotRender.dfa(dfa, title: 'DFA', sink_explicit: true, q: 'r'))
  puts "Wrote #{outdir}/dfa.dot"
  system("dot -Tsvg #{outdir}/dfa.dot -o #{outdir}/dfa.svg")
  File.write(File.join(outdir, 'dfa_implicit_sink.dot'), DotRender.dfa(dfa, title: 'DFA_ImplicitSink', sink_explicit: false))
  system("dot -Tsvg #{outdir}/dfa_implicit_sink.dot -o #{outdir}/dfa_implicit_sink.svg")

  # Minimization with trace (table method)
  min, min_trace = DFAMinimizer.minimize_with_trace(dfa)
  File.write(File.join(outdir, 'min_dfa.dot'), DotRender.dfa(min, title: 'MinDFA', sink_explicit: true, q: 's'))
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
