module TrustedTemplate
    CONTENT_SLOT = '#{CONTENT}'

    # Evaluate interpolation directives that are part of a trusted template file.
    # Evaluation results are deliberately appended as opaque strings: they must
    # never be parsed again, because they may contain user-controlled content.
    def self.render(source, context)
        output = +''
        offset = 0

        while (index = source.index('#{', offset))
            output << source[offset...index]
            length = interpolation_length(source, index)
            code = source[index + 2, length - 3]

            if code.start_with?('<')
                output << '&#35;{' << code << '}'
            else
                output << (eval(code, context).to_s || '')
            end
            offset = index + length
        end

        output << source[offset..]
        output
    end

    # Render the trusted page and layout independently. The page result is only
    # inserted after both trusted sources have been evaluated, preventing output
    # such as student submissions from becoming executable template syntax.
    def self.render_page(layout, page, context)
        before, slot, after = layout.partition(CONTENT_SLOT)
        raise ArgumentError, 'Layout is missing #{CONTENT}' if slot.empty?
        raise ArgumentError, 'Layout contains multiple #{CONTENT} slots' if after.include?(CONTENT_SLOT)

        render(before, context) + render(page, context) + render(after, context)
    end

    def self.interpolation_length(source, index)
        length = 2
        balance = 1
        while index + length < source.size && balance > 0
            character = source[index + length]
            balance -= 1 if character == '}'
            balance += 1 if character == '{'
            length += 1
        end
        raise ArgumentError, 'Unclosed template interpolation' unless balance.zero?

        length
    end
    private_class_method :interpolation_length
end
