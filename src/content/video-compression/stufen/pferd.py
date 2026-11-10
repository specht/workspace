import numpy as np
from PIL import Image

from video import write_video

plate = np.asarray(Image.open('muybridge.jpg').convert('L'))
frames = []
for k in range(11):
    row, column = divmod(k, 4)
    y = 40 + 218 * row
    x = 36 + 335 * column
    frames.append(plate[y:y + 200, x:x + 316])
frames = np.stack(frames)
rgb = np.stack([frames, frames, frames], axis=-1)
write_video('pferd.mp4', np.tile(rgb, (10, 1, 1, 1)), fps=25)
