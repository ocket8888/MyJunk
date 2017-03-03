#!/usr/bin/env python3

import argparse
import datetime as dt
from math import log
from sys import stdin, stdout, stderr


#Figure out where the data's coming from
parser = argparse.ArgumentParser(description="Displays data exported in ASCII format from Maestro software.")

parser.add_argument("FILE", type=str, nargs='?', default=None, help="The name of the file to read from. If not given, reads from stdin.")
args=parser.parse_args()

if args.FILE:
	infile=open(args.FILE)
else:
	infile=stdin

contents = infile.read().strip().split("\n")


#read the data into its fields
fields = dict()
currentField = ""
for line in contents:
	if line[0] == '$':
		currentField = line[1:len(line)-1:]
		fields[currentField]=list()
		continue
	fields[currentField].append(line)

#parse the easy fields
sample_description = fields["SPEC_ID"][0]
measurement_time = fields["MEAS_TIM"][0].split(" ")
measurement_time = {"live": int(measurement_time[0]), "real": int(measurement_time[1])}
date_time = dt.datetime.strptime(fields["DATE_MEA"][0], "%m/%d/%Y %H:%M:%S")

#parse the collected data
data = fields["DATA"]
minMax = [int(x) for x in data.pop(0).split(" ")]
data = [int(counts.strip()) for counts in data ]
countsMax = max(data)

#check for ROI's and read them in if applicable
roi = fields["ROI"]
roiNum = int(roi.pop(0))
regions = list()
for region in roi:
	bounds = region.split(" ")
	regions.append((int(bounds[0]), int(bounds[1])))
	

#start the display
import pygame
from pygame.locals import *
if not pygame.font:
	print("Warning, fonts disabled")

screenSize = (1024, 576)
white = (255, 255, 255)
black = (0, 0, 0)
limeGreen = (50, 205, 50)
paleBlue = (145, 163, 210)

pygame.init()
screen = pygame.display.set_mode(screenSize)
screen.fill(white)
pygame.display.update()

#Currently only use this button to switch between linear scale and (broken) log scale
#useful for other things though, so it's a class
class Button:
	def __init__(self, surface, color, x, y, width, height, text, text_color):
		self.x = x
		self.y = y
		self.height = height
		self.width = width
		self.color = color
		self.text_color = text_color
		self.parentSurface = surface
		self.text = text
		self.buttonSurface = self.draw_button()
		self.write_text()
		self.rect = pygame.Rect(x,y, width, height)

	def write_text(self):
		font_size = self.width//len(self.text)+5 #This is pretty arbitrary
		myFont = pygame.font.SysFont("Calibri", font_size)
		myText = myFont.render(self.text, 1, self.text_color)
		self.buttonSurface.blit(myText, ((self.width//2) - myText.get_width()//2, (self.height//2) - myText.get_height()//2))

	def draw_button(self):
		print(self.width, self.height)
		surface = pygame.Surface((self.width, self.height))
		surface.fill(self.color)
		pygame.draw.rect(surface, (190,190,190), (1,1,self.width-1,self.height-1), 1)
		self.parentSurface.blit(surface, (self.x, self.y))
		return surface

	def repaint(self, newText):
		if newText == None:
			newText = self.text
		self.buttonSurface.fill(self.color)
		pygame.draw.rect(self.buttonSurface, (190,190,190), (1,1,self.width-1,self.height-1), 1)
		font_size = self.width//len(newText)+5
		myFont = pygame.font.SysFont("Calibri", font_size)
		myText = myFont.render(newText, 1, self.text_color)
		self.buttonSurface.blit(myText, ((self.width//2) - myText.get_width()//2, (self.height//2) - myText.get_height()//2))
		self.text = newText
		self.parentSurface.blit(self.buttonSurface, (self.x, self.y))


	def pressed(self, mouse):
		if mouse[0] > self.rect.topleft[0]:
			if mouse[1] > self.rect.topleft[1]:
				if mouse[0] < self.rect.bottomright[0]:
					if mouse[1] < self.rect.bottomright[1]:
						return True
		return False

#define the graph's draw area
graphSize = (900, 450)
graphArea = pygame.Surface(graphSize)
graphOffset = (62, 106) #left offset, top offset)
screen.blit(graphArea, graphOffset)

#This function draws an arbitrary spectrum
def drawSpectrum(spectrum):
	binNumber = 0
	specMax =  max(spectrum)
	graphSurface = pygame.Surface(graphSize)
	graphSurface.fill(white)
	for Bin in spectrum:
		binColor = ( 255 - int((specMax - Bin)*(255.0/specMax)), 32, int((specMax - Bin)*(255.0/specMax)))
		for region in regions:
			if Bin in range(region[0], region[1]+1):
				binColor = limeGreen
				break
		pygame.draw.rect(graphSurface, binColor, (int(binNumber*(graphSize[0]/len(spectrum))), graphSize[1]-(Bin*graphSize[1]//specMax), (graphSize[0]/len(spectrum))-1, Bin*graphSize[1]//specMax))
		binNumber += 1
	return graphSurface

#This function toggles linear and logarithmic displays, for button use
def linLogToggle(linear, spectrum):
	if linear:
		return drawSpectrum(spectrum)
	return drawSpectrum([int(log(count + 1, 10)) for count in spectrum]) #adds 1 to avoid exploding on 0 count bins

#Checks if a mouse click happens inside a surface
def inObject(mouse, obj):
	objRect = obj.get_rect() #more like get rekt lol
	if mouse[0] > objRect.topleft[0]:
		if mouse[1] > objRect.topleft[1]:
			if mouse[0] < objRect.bottomright[0]:
				if mouse[1] < objRect.bottomright[1]:
					return True
	return False

def selectBins(dragStart, dragEnd, spec):
	print("Dragged from "+repr(dragStart)+ " to "+repr(dragEnd))

graph = drawSpectrum(data).convert()
buttonText = "Log Scale"
linLogButton = Button(screen, limeGreen, 15, 15, 100, 50, buttonText, black)

currentSpectrum = data #always preserves linear scale, so we don't lose resolution
dragging = False

#main loop
while True:
	graphArea.fill(white)
	for event in pygame.event.get():
		if event.type == pygame.QUIT:
			pygame.display.quit()
			pygame.quit()
			exit()
		elif event.type == MOUSEBUTTONDOWN:
			mousePos = pygame.mouse.get_pos()
			if linLogButton.pressed(mousePos):
				if buttonText == "Linear Scale":
					graph = linLogToggle(True, currentSpectrum).convert()
					buttonText = "Log Scale"
				else:
					graph = linLogToggle(False, currentSpectrum).convert()
					buttonText = "Linear Scale"
			elif inObject(mousePos, graph):
				draggingX, draggingY = mousePos
				draggingX -= graphOffset[0]
				dragging = (draggingX, draggingY)
				print(dragging)
		elif event.type == MOUSEBUTTONUP:
			mousePos = pygame.mouse.get_pos()
			selectBins(dragging[0], mousePos[0], currentSpectrum)
			dragging = False
	
	graphArea.blit(graph, (0, 0))

	if dragging:
		currentMouseX, currentMouseY = pygame.mouse.get_pos()
		currentMouseX -= graphOffset[0]
		dragSurface = pygame.Surface((abs(dragging[0] - currentMouseX), graphSize[1])).convert()
		dragSurface.set_alpha(70)
		dragSurface.fill(paleBlue)
		graphArea.blit(dragSurface, (min((dragging[0], currentMouseX)), 0))
			
	linLogButton.repaint(buttonText)
	screen.blit(graphArea, graphOffset)
	pygame.display.update()
