module SerializedNeo4j
    def initialize(...)
        @neo4j_query_mutex = Mutex.new
        super
    end

    def neo4j_query(...)
        @neo4j_query_mutex.synchronize { super }
    end
end
