#!/usr/bin/python
import sys
import json
import argparse

parser = argparse.ArgumentParser(
    prog='dkb-giro.py', description='parse transaction information or balance information from output of dkb-giro.js')

parser.add_argument('--action', dest='action', default='transactions', help='action (transactions [default] or balance)')

args = parser.parse_args()
if args.action == "transactions":
    x = []

    for line in sys.stdin:
      line = line.replace('"', '')
      fields = line.split(';')

      if len(fields) == 12 and fields[0] != "Buchungstag" :
        dateParts = fields[1].split('.')
        valueDate = dateParts[2] + "-" + ("0" + dateParts[1])[-2:] + "-" + ("0" + dateParts[0])[-2:]

        dateParts = fields[0].split('.')
        bookingDate = dateParts[2] + "-" + ("0" + dateParts[1])[-2:] + "-" + ("0" + dateParts[0])[-2:]

        amount = -1*float(fields[7].replace('.', '').replace(',', '.'))

        text = fields[2] + " " + fields[3] + " " + fields[4] + " " + fields[5] + " " + fields[6]
        text += " " + fields[8] + " " + fields[9]  + " " + fields[10]  

        text= " ".join(text.split()).replace('<br />', '\n')

        x.append({'valueDate': valueDate, 'bookingDate': bookingDate, 'amount': amount, 'text': text, 'preliminary': False})

    # oldest first
    x.reverse()

    print(json.dumps(x, indent=' '))
elif args.action == 'balance':
    for line in sys.stdin:
      line = line.replace('"', '')
      fields = line.split(';')

      if fields[0].startswith('Kontostand vom '):
          amount = fields[1].split(' ')[0]
          print(float(amount.replace(',', '')))
          exit(0)

    print("no 'Kontostand vom' field found in input!", file=sys.stderr)
    exit(1)
else:
    print("unknown action: " + args.action, file=sys.stderr)
    exit(2)
