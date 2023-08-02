with open("app/Makefile") as doc:
    txt = doc.read()
parts = txt.split('bnfc')

with open("app/Makefile2", "w+") as doc:
    doc.write(parts[0] + "bnfc" + parts[1] + " /home/students/inf/PUBLIC/MRJP/bin/bnfc" + parts[2])
