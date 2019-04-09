from PIL import Image, ImageDraw

f = open("morphing1.txt")

a = []
b = []

for line in f.readlines():
    t = [int(x) for x in line.strip().split(' ')]
    if (len(t) == 2):
        a.append(t)
    else:
        b.append(t)

im = Image.open("source1.png")
draw = ImageDraw.Draw(im)

for tri in b:
    x, y, z = tri
    draw.line((a[x][0], a[x][1], a[y][0], a[y][1]) ,width=3)
    draw.line((a[x][0], a[x][1], a[z][0], a[z][1]) ,width=3)
    draw.line((a[z][0], a[z][1], a[y][0], a[y][1]) ,width=3)

im.save("delaunay1.png")