'use strict';

import moment from 'moment/src/moment.js';
import page from './statements.html';
import $ from '/jquery.js';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';

export let location;

let dateModified = false;
let dateFrom = moment().subtract(1, 'month');
let dateTo = moment();

let accountId = null;
let table;

export function refresh() {
  if (!dateModified) {
    dateFrom = moment().subtract(1, 'month');
    dateTo = moment();
  }

  $('#date-from', location).calendar('set date', dateFrom.format('LLL'), true, false);
  $('#date-to', location).calendar('set date', dateTo.format('LLL'), true, false);

  if (accountId == null)
    table.clear().draw();
  else {
    let expOpts = {
      'draw': 1,
      'columns': [{
          'data': 'id',
          'name': '',
          'searchable': true,
          'orderable': true,
          'search': {
            'value': '',
            'regex': false
          }
        },
        {
          'data': 'valueDate',
          'name': '',
          'searchable': true,
          'orderable': true,
          'search': {
            'value': '',
            'regex': false
          }
        },
      ],
      'order': [{
          'column': 1,
          'dir': 'desc'
        },
        {
          'column': 0,
          'dir': 'desc'
        }
      ],
      'start': 0,
      'length': 0,
      'search': {
        'value': '',
        'regex': false
      },
      'extra': {
        'accounts': [accountId],
        'categories': [],
        'from': dateFrom.toISOString(),
        'to': dateTo.toISOString()
      }
    };

    let balOpts = _.cloneDeep(expOpts);
    delete balOpts.extra['categories'];
    balOpts.columns[1].data = 'date';

    $.when(Api.postExpensesRenderedQuery(expOpts), Api.postBalancesQuery(balOpts), Api.getComputeSimulationByTime(dateFrom.toISOString(), 'Past')).then(function(ejson, bjson, sim) {
      ejson = ejson[0];
      bjson = bjson[0];

      sim = _.find(sim[0].balances, x => x.accountId == accountId);
      sim.relevantAmount = 0;
      sim.balance = sim.amount;
      sim.id = '';
      sim.store = '';
      sim.title = 'Kontostand vom ' + moment(sim.base.date).format('LL');
      sim.valueDate = sim.base.date;
      sim.bookingDate = null;
      if (sim.steps > 0)
        sim.title += ' + ' + (sim.steps > 1 ? sim.steps : 'eine') + ' weitere Ausgabe' + (sim.steps > 1 ? 'n' : '');

      let es = _.map(ejson.data, function(x) {
        let y = Util.flattenRecord(x);
        y.relevantAmount = 0.0;

        if (y.accountId == accountId)
          y.relevantAmount -= y.amount;

        _.forEach(y.sharing, function(z) {
          if (z.sharingAccountId == accountId)
            y.relevantAmount -= z.calculatedAmount;
        });

        y.balance = null;

        return y;
      });
      _.remove(es, z => z.relevantAmount == 0);

      let bs = _.map(bjson.data, function(x) {
        let y = Util.flattenRecord(x);
        y.relevantAmount = 0;
        y.balance = y.amount;
        y.valueDate = y.date;
        y.bookingDate = null;
        y.id = '';
        y.store = '';
        y.title = 'Kontostand';

        return y;
      });

      let d = [];
      d.push(...es);
      d.push(...bs);
      d.push(sim);
      d = _.sortBy(d, x => x.valueDate);
      _.reverse(d);

      for (let i = d.length - 2; i >= 0; i--) {
        if (d[i].balance == null)
          d[i].balance = d[i + 1].balance + d[i].relevantAmount;
      }

      let csvContent = 'data:text/csv;charset=utf-8,';
      d.forEach(function(x) {
        let bD = x.bookingDate == null ? '' : moment(x.bookingDate).toISOString();
        let row = [moment(x.valueDate).toISOString(), bD, x.amount, x.balance, x.title, x.store];
        csvContent += row.join(',') + '\n';
      });

      let encodedUri = encodeURI(csvContent);
      $('#csv-export', location).attr('href', encodedUri)
        .attr('download', 'statement-' + accountId + '.csv')
        .removeClass('disabled');

      table.clear().rows.add(d).draw();
    });
  }
}

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  $('#date-from', location).calendar({
    // type: 'date',
    today: true,
    ampm: false,
    firstDayOfWeek: 1,
    formatter: {
      datetime: function(d) {
        return moment(d).format('L');
      }
    },
    parser: {
      datetime: function(d) {
        return moment(d, 'L').toDate();
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
    onChange: function(date) {
      dateFrom = moment(date);
      dateModified = true;
      refresh();
    }
  });

  $('#date-to', location).calendar({
    // type: 'date',
    today: true,
    ampm: false,
    firstDayOfWeek: 1,
    formatter: {
      datetime: function(d) {
        return moment(d).format('L');
      }
    },
    parser: {
      datetime: function(d) {
        return moment(d, 'L').toDate();
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
    onChange: function(date) {
      dateTo = moment(date);
      dateModified = true;
      refresh();
    }
  });

  Common.makeAccountDropdown($('#account', location), false, false);

  $('#account', location).parent().dropdown('refresh');
  $('#account', location).change(function() {
    accountId = parseInt($(this).val());
    refresh();
  });

  table = $('#table-statements', location).DataTable({
    'paging': true,
    'ordering': false,
    'info': false,
    'pageLength': 50,
    // 'lengthMenu': [20, 50, 100],
    'dom': 'rtip',
    'data': [],
    'language': Util.datatableLanguage,
    createdRow: function(row, data) {
      $('td:nth-child(3)', row).addClass('right aligned');
      $('td:nth-child(4)', row).addClass('right aligned');

      if (data.relevantAmount > 0)
        $('td:nth-child(3)', row).addClass('positive');
      else if (data.relevantAmount < 0)
        $('td:nth-child(3)', row).addClass('negative');
    },
    columns: [
      // {
      //   title: 'ID',
      //   data: 'id',
      //   width: '2.25em'
      // },
      {
        title: 'Wertstellungsdatum',
        data: 'valueDate',
        width: '4.5em',
        render: function(data) {
          return moment(data).format('L');
        }
      },
      {
        title: 'Buchungsdatum',
        data: 'bookingDate',
        width: '4.5em',
        render: function(data) {
          if (data == null) return '';
          return moment(data).format('L');
        }
      },
      {
        title: 'Betrag',
        data: 'relevantAmount',
        width: '5em',
        orderable: false,
        render: function(data) {
          if (data != 0)
            return Util.formatAmount(data);
          else return '';
        }
      }, {
        title: 'Saldo',
        data: 'balance',
        width: '5.5em',
        orderable: false,
        render: function(data) {
          return Util.formatAmount(data);
        }
      },
      {
        title: 'Titel',
        data: 'title'
      },
      {
        title: 'Gesch&auml;ft',
        data: 'store',
        width: '14em'
      }
    ]
  });

  refresh();
}
