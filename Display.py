import pygame
from pygame import constants

pygame.init()
WIDTH = 600
HEIGHT = 800
screen = pygame.display.set_mode((HEIGHT, WIDTH), flags=constants.RESIZABLE)

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.quit():
            running = False
