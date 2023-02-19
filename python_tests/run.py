import tne
import numpy as np

game = tne.TnEGame()
print(game.state())

# game.process(action, from_x, from_y, to_x, to_y, red, green, blue, black, leader)