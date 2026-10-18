# Für die Bilder der Seite (Rezepte mit image-file): lädt man diese Datei mit
# »ruby -r«, speichert das Programm nach seinem Block, was es gezeichnet hat,
# in step.png (oder STEP_PNG), und der Zufall fängt immer gleich an, damit
# der Himmel jedes Mal derselbe ist. Die Programme selbst bleiben, wie die
# Seite sie zeigt.
require 'pixelflow_canvas'

srand(Integer(ENV.fetch('STEP_SEED', '7')))

module SaveStep
    def initialize(*args, &block)
        super
        save_as_png(ENV.fetch('STEP_PNG', 'step.png'))
    end
end
Pixelflow::Canvas.prepend(SaveStep)
