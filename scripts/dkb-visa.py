#!/usr/bin/python
import sys
import json
import argparse

parser = argparse.ArgumentParser(
    prog='dkb-visa.py', description='parse transaction information or balance information from output of dkb-visa.js')

parser.add_argument('--action', dest='action', default='transactions', help='action (transactions [default] or balance)')

args = parser.parse_args()
if args.action == "transactions":
    x = []

    for line in sys.stdin:
      line = line.replace('"', '')
      fields = line.split(';')

      if fields[0] == "Nein" or fields[0] == "Ja" :
        dateParts = fields[1].split('.')
        valueDate = dateParts[2] + "-" + ("0" + dateParts[1])[-2:] + "-" + ("0" + dateParts[0])[-2:]

        dateParts = fields[2].split('.')
        bookingDate = dateParts[2] + "-" + ("0" + dateParts[1])[-2:] + "-" + ("0" + dateParts[0])[-2:]

        amount = -1*float(fields[4].replace('.', '').replace(',', '.'))
        text = " ".join(fields[3].split())

        if len(fields[5]) > 0:
            if (fields[5][0] == '-'):
                fields[5] = fields[5][1:]
            else:
                fields[5] = "-" + fields[5]

            text += "; Originalbetrag: " + fields[5]

        x.append({'valueDate': valueDate, 'bookingDate': bookingDate, 'amount': amount, 'text': text, 'preliminary': False})

    # oldest first
    x.reverse()

    print(json.dumps(x, indent=' '))
elif args.action == 'balance':
    for line in sys.stdin:
      line = line.replace('"', '')
      fields = line.split(';')

      if fields[0] == 'Saldo:':
          amount = fields[1].split(' ')[0]
          print(float(amount.replace(',', '')))
          exit(0)

    print("no 'Saldo' field found in input!", file=sys.stderr)
    exit(1)
else:
    print("unknown action: " + args.action, file=sys.stderr)
    exit(2)
