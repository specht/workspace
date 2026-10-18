# Für die Bilder der Seite (Rezepte mit image-file): lädt man diese Datei mit
# »ruby -r«, speichert das Programm, was es gezeichnet hat, in step.png:
# ein Programm ohne Schleife nach seinem Block, die Animation nach
# STEP_FRAMES Bildern (flip), danach hört sie auf. Der Zufall fängt immer
# gleich an, damit das Feuer jedes Mal dasselbe ist. Die Programme selbst
# bleiben, wie die Seite sie zeigt.
require 'pixelflow_canvas'

srand(Integer(ENV.fetch('STEP_SEED', '7')))

module SaveStep
    def initialize(*args, &block)
        super
        save_as_png(ENV.fetch('STEP_PNG', 'step.png'))
    end

    def flip(*args)
        super
        @frames = (@frames || 0) + 1
        return if @frames < Integer(ENV.fetch('STEP_FRAMES', '200'))

        save_as_png(ENV.fetch('STEP_PNG', 'step.png'))
        exit
    end
end
Pixelflow::Canvas.prepend(SaveStep)
