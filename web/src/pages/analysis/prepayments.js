'use strict';

import moment from 'moment/src/moment.js';
import page from './prepayments.html';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';

export let location;

let dateModified = false;
let date = moment();

let formatAmountTd = function(x) {
  if (typeof x == 'undefined' || x == null)
    return '<td class="error"><i class="attention icon"></i> NaN</td>';
  else
    return '<td class="' + (x < 0 ? 'negative' : 'positive') + '">' + Util.formatAmount(x) + '</td>';
};

export function refresh() {
  if (!dateModified)
    date = moment();

  $('#date', location).calendar('set date', date.format('LLL'), true, false);

  $.when(Api.getComputePrepaymentsByTime(date.toISOString())).then(function(ep) {
    let tp = $('#table-prepayments tbody', location);
    tp.empty();

    if (ep.length == 0)
      tp.append('<tr><td></td><td></td><td>Keine Vorauszahlungen!</td></tr>');

    let total = 0.0;

    ep.forEach(function(row) {
      let s = '<tr><td>' + row.title + '</td>' + formatAmountTd(-1 * row.amount);
      s += '<td>' + moment(row.date).format('L');

      if (row.expenses.length == 2)
        s += '  + eine weitere';
      else if (row.expenses.length > 2)
        s += '  + ' + (row.expenses.length - 1) + ' weitere';

      s += '</td></tr>';
      tp.append(s);

      total -= row.amount;
    });

    $('#table-prepayments tfoot', location).html('<tr><th>Gesamt</th>' + formatAmountTd(total).replace('td', 'th') + '<th></th></tr>');
  }, function(e) {
    Util.errorMsg('Anfrage von Vorauszahlungen schlug fehl', e);
  });
}

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  $('#date', location).calendar({
    // type: 'date',
    today: true,
    ampm: false,
    firstDayOfWeek: 1,
    formatter: {
      datetime: function(d) {
        return moment(d).format('LLL');
      }
    },
    parser: {
      datetime: function(d) {
        return moment(d, 'LLL').toDate();
      }
    },
    text: {
      days: ['S', 'M', 'D', 'M', 'D', 'F', 'S'],
      months: ['Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'],
      monthsShort: ['Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'],
      today: 'Heute',
      now: 'Jetzt',
      am: 'AM',
      pm: 'PM'
    },
    onChange: function(d) {
      date = moment(d);
      dateModified = true;
      refresh();
    }
  });

  refresh();
}
