'use strict';

import moment from 'moment/src/moment.js';
import page from './computed_balances.html';
import tinycolor from 'tinycolor2';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as Sunburst from './sunburst.js';

export let date = moment();
export let location;
export let data;

let dateModified = false;

let formatAccountTd = function(aid) {
  let a = Common.findAccount(aid);

  return '<td style="background-color:' + a.color + ';' + (tinycolor(a.color).getBrightness() < 100 ? 'color:#F7F7F7;' : '') + '">' + a.title + '</td>';
};

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

  $.when(Api.getComputeSimulationByTime(date.toISOString(), 'Both')).then(function(eb) {
    data = eb;

    // one could make the sign switchable via button / radio ...
    Sunburst.createSmall($('#sunburst', location), Sunburst.balanceData(eb, 'Verfügbar', 1));

    let tb = $('#table-balances tbody', location).first();
    tb.empty();

    eb.balances.forEach(function(row) {
      if (typeof row.amount === 'undefined') {
        tb.append('<tr>' + formatAccountTd(row.accountId) + formatAmountTd() + '<td class="negative">Keine Daten</td></tr>');
      } else {
        tb.append('<tr>' + formatAccountTd(row.accountId) + formatAmountTd(row.amount) + '<td>Stand ' + moment(row.base.date).from(date) + ' (am ' + moment(row.base.date).format('L LT') + ') ' + Util.formatAdditions(row.steps) + '</td></tr>');
      }
    });

    $('#table-balances tfoot', location).html('<tr><th>Gesamt</th>' + formatAmountTd(eb.total[eb.total.length - 1].amount).replace('td', 'th') + '<th></th></tr>');

    // availabilites
    let ta = $('#table-availabilities tbody', location).first();
    ta.empty();

    eb.total.forEach(function(av) {
      ta.append('<tr><td>' + Util.availabilityToString(av.when) + '</td>' + formatAmountTd(av.amount) + '</td></tr>');
    });
  }, function(e) {
    Util.errorMsg('Anfrage von simulierten Kontoständen schlug fehl', e);
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
