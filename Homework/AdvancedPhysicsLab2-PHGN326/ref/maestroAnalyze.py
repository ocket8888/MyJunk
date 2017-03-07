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
paleTurquoise = (175, 255, 222)

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

class Infobox:
	"""Displays info about a selected region"""
	def __init__(self, x, y, bkgrndColor):
		self.x = x
		self.y = y
		self.bkgrndColor = bkgrndColor
		self.NetArea = 0
		self.NetAreaError = 0
		self.FWHM = 0

		

#define the graph's draw area
graphSize = (900, 450)
graphArea = pygame.Surface(graphSize)
graphOffset = (62, 106) #left offset, top offset)
screen.blit(graphArea, graphOffset)

#This function draws an arbitrary spectrum
def drawSpectrum(spectrum, boundaries):
	binNumber = 0
	specMax =  max(spectrum)
	graphSurface = pygame.Surface(graphSize)
	graphSurface.fill(white)
	for Bin in spectrum:
		binColor = ( 255 - int((specMax - Bin)*(255.0/specMax)), 32, int((specMax - Bin)*(255.0/specMax)))
		for region in regions:
			if binNumber + boundaries[0] in range(region[0], region[1]+1):
				binColor = limeGreen
				break
		pygame.draw.rect(graphSurface, binColor, (int(binNumber*(graphSize[0]/len(spectrum))), graphSize[1]-(Bin*graphSize[1]//specMax), (graphSize[0]/len(spectrum))-1, Bin*graphSize[1]//specMax))
		binNumber += 1
	return graphSurface

#This function toggles linear and logarithmic displays, for button use
def linLogToggle(linear, spectrum, boundary):
	if linear:
		return drawSpectrum(spectrum, boundary)
	return drawSpectrum([log(count + 1, 10) for count in spectrum], boundary) #adds 1 to avoid exploding on 0 count bins

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
	leftside, rightside = min((dragStart, dragEnd)), max((dragStart, dragEnd))
	draggedSize = (rightside - leftside, graphSize[1])
	draggedSurface = pygame.Surface(draggedSize)
	draggedSurface.set_alpha(100)
	draggedSurface.fill(paleBlue)
	draggedBins = (leftside*len(spec)//graphSize[0], rightside*len(spec)//graphSize[0])
	return (draggedSurface, leftside, draggedBins)

#Yes this is basically just a list comprehension, yes doing it this way is probably slower
#No, I don't care. It's way easier to read this way.
def calculateSelectableRegions(boundaries):
	canSelect = list()
	for region in regions:
		if region[0] in range(boundaries[0], boundaries[1]+1) and region[1] in range(boundaries[0], boundaries[1]+1):
			canSelect.append(region)
	return canSelect

def selectROI(currentROI, boundaries, spectrumSize, selectNext=False):
	if len(selectableRegions) == 0:
		return False, None

	if selectNext:
		if currentROI == None:
			newlySelectedROI = 0
		else:
			newlySelectedROI = (currentROI + 1) % len(selectableRegions)
	else:
		if currentROI == None or currentROI == 0:
			newlySelectedROI = len(selectableRegions) - 1
		else:
			newlySelectedROI = currentROI -1

	regionData = selectableRegions[newlySelectedROI]

	draggedSize = ((regionData[1] - regionData[0]+1)*graphSize[0]//spectrumSize, graphSize[1])
	draggedSurface = pygame.Surface(draggedSize)
	draggedSurface.set_alpha(100)
	draggedSurface.fill(paleTurquoise)
	
	leftside = (regionData[0] - boundaries[0])*graphSize[0]//spectrumSize

	return (draggedSurface, leftside, regionData), newlySelectedROI




graph = drawSpectrum(data, minMax).convert()
buttonText = "Log Scale"
linLogButton = Button(screen, limeGreen, 15, 15, 100, 50, buttonText, black)

currentSpectrum = data #always preserves linear scale, so we don't lose resolution
currentBounds = minMax
dragging = False
dragged = False
unzoom = False
selectedROI = None
selectableRegions = regions


#main loop
while True:
	screen.fill(white)
	graphArea.fill(white)
	for event in pygame.event.get():

		#handle window close (fucking sort of)
		if event.type == pygame.QUIT:
			pygame.display.quit()
			pygame.quit()
			exit()

		#handle mouse clicks
		elif event.type == MOUSEBUTTONDOWN:
			mousePos = pygame.mouse.get_pos()

			#check for user toggling linear and log scales
			if linLogButton.pressed(mousePos):
				if buttonText == "Linear Scale":
					graph = linLogToggle(True, currentSpectrum, currentBounds).convert()
					buttonText = "Log Scale"
				else:
					graph = linLogToggle(False, currentSpectrum, currentBounds).convert()
					buttonText = "Linear Scale"

			#if an area is drag selected, check if the user pushes the "Zoom" button
			elif dragged and zoomButton.pressed(pygame.mouse.get_pos()):
				print(currentBounds)
				currentBounds = (currentBounds[0] + dragged[2][0], currentBounds[0] + dragged[2][1])
				print(currentBounds)
				currentSpectrum = currentSpectrum[dragged[2][0]:dragged[2][1]+1:]
				dragged = False
				selectedROI = None
				print(selectableRegions)
				selectableRegions = calculateSelectableRegions(currentBounds)
				print(selectableRegions)
				graph = drawSpectrum(currentSpectrum, currentBounds).convert()
				unzoom = True
				unzoomButton = Button(screen, paleBlue, screenSize[0]-240, 15, 100, 50, "Zoom Out", black)

			#if spectrum is zoomed, check if the user pushes the "Zoom Out" button
			elif unzoom and unzoomButton.pressed(pygame.mouse.get_pos()):
				currentSpectrum = data
				currentBounds = minMax
				selectableRegions = regions
				unzoom = False
				dragged = False
				graph = drawSpectrum(currentSpectrum, currentBounds).convert()
				buttonText = "Log Scale"

			#if the user clicks in the graph area, start a drag select
			elif inObject(mousePos, graphArea):
				dragged = False
				selectedROI = None
				draggingX, draggingY = mousePos
				draggingX -= graphOffset[0]
				dragging = (draggingX, draggingY)

		#handles when the mousebutton is released (currently only use is for ending a drag-select)
		elif event.type == MOUSEBUTTONUP and dragging:
			mousePos = pygame.mouse.get_pos()
			dragged = selectBins(dragging[0], mousePos[0] - graphOffset[0], currentSpectrum)
			dragging = False
			zoomButton = Button(screen, paleBlue, screenSize[0] - 120, 15, 100, 50, "Zoom", black)

		#handle key presses
		elif event.type == KEYDOWN:
			keys = pygame.key.get_pressed()
			if keys[K_RIGHT]:
				dragging = False
				dragged, selectedROI = selectROI(selectedROI, currentBounds, len(currentSpectrum), selectNext=True)
			elif keys[K_LEFT]:
				dragging = False
				dragged, selectedROI = selectROI(selectedROI, currentBounds, len(currentSpectrum))

			if dragged:
				zoomButton = Button(screen, paleBlue, screenSize[0] - 120, 15, 100, 50, "Zoom", black)
	
	graphArea.blit(graph, (0, 0))

	if dragging:
		currentMouseX, currentMouseY = pygame.mouse.get_pos()
		currentMouseX -= graphOffset[0]
		dragSurface = pygame.Surface((abs(dragging[0] - currentMouseX), graphSize[1])).convert()
		dragSurface.set_alpha(70)
		dragSurface.fill(paleBlue)
		graphArea.blit(dragSurface, (min((dragging[0], currentMouseX)), 0))
	elif dragged:
		graphArea.blit(dragged[0].convert(), (dragged[1], 0))
		zoomButton.repaint(None)

	if unzoom:
		unzoomButton.repaint(None)
			
	linLogButton.repaint(buttonText)
	screen.blit(graphArea, graphOffset)
	pygame.display.update()
