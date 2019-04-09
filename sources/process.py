a = ["source1", "target1", "source2"]

for name in a:
    f1 = open(name + ".json", "r")
    f2 = open(name + ".txt", "w")
    y = 0
    x = 0
    for line in f1.readlines():
        if line.strip(" ,\n").find("\"y\":") != -1:
            y = float(line.split(": ")[1].strip(",\n"))
        if line.strip(" ,\n").find("\"x\":") != -1:
            x = float(line.split(": ")[1].strip(",\n"))
            f2.write("({},{})\n".format(x,y))
    f1.close()
    f2.close()