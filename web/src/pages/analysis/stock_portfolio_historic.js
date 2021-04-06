'use strict';

import moment from 'moment/src/moment.js';
import page from './stock_portfolio_historic.html';
import $ from '/jquery.js';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';

export let location;

let dateModified = false;
let date = moment();
let table;

export function refresh() {
  if (!dateModified) {
    date = moment();
  }

  $('#date', location).calendar('set date', date.format('LLL'), true, false);

  $.when(Common.getStocks(), Api.getComputeStocksPortfolioHistoric(date.format('YYYY-MM-DD'))).then((_s, x) => {
    x = x[0];

    let invested = 0.0;
    let value = 0.0;
    let diff = 0.0;
    let diffDay = 0.0;

    let stocks = _.map(x.stocks, y => {
      y.stock = Common.findStock(y.stockId);
      invested += y.invested;

      if (y.value !== null && y.previousValue !== null && y.previousValue != 0.0) {
        let perc = (y.value - y.previousValue) / y.previousValue;
        y.diffDay = y.value - y.previousValue;
        y.diffDayS = Util.formatAmount(y.value - y.previousValue) + ' (' + Util.formatPercentage(perc) + ')<br /> zum ' + moment(y.previousDate).format('L');

        if (diffDay !== null) diffDay += y.diffDay;
      } else {
        y.diffDay = null;
        y.diffDayS = null;
        diffDay = null;
      }

      if (y.value !== null && y.invested != 0.0) {
        let perc = (y.value - y.invested) / y.invested;
        y.diff = y.value - y.invested;
        y.diffS = Util.formatAmount(y.value - y.invested) + ' (' + Util.formatPercentage(perc) + ')';
        if (diff !== null) diff += y.diff;
      } else {
        y.diff = null;
        y.diffS = null;
        diff = null;
      }

      if (y.value !== null) {
        let ex = _.head(_.filter(y.stock.exchanges, x => x.id == y.exchangeId));
        y.valueS = Util.formatAmount(y.value) + '<br /> am ' + moment(y.date).format('L') + ' (' + ex.name + ')';

        if (value !== null) value += y.value;
      } else {
        y.valueS = null;
        value = null;
      }

      return y;
    });

    table.clear().rows.add(stocks).draw();

    let valueS = '';
    if (value !== null) {
      valueS = Util.formatAmount(value);
    }

    let investedS = Util.formatAmount(invested);

    let diffS = '';
    if (diff !== null) {
      diffS = Util.formatAmount(diff);

      if (invested != 0.0)
        diffS += ' (' + Util.formatPercentage(diff / invested) + ')';
    }

    let diffDayS = '';
    if (diffDay !== null) {
      diffDayS = Util.formatAmount(diffDay);
      if (value - diffDay != 0.0)
        diffDayS += ' (' + Util.formatPercentage(diffDay / (value - diffDay)) + ')';
    }

    let irrS = '';
    if (x.portfolioIrr !== null) {
      irrS = Util.formatPercentage(x.portfolioIrr) + ' p.a.';
    }

    $('#table-portfolio tfoot', location).html('<tr><th>Gesamt</th><th></th><th></th><th class="right aligned">' + investedS + '</th><th class="right aligned">' + valueS + '</th><th class="right aligned">' + diffDayS + '</th><th class="right aligned">' + diffS + '</th><th class="right aligned">' + irrS + '</th></tr>');
  });
}

let showDetails = function(rowjq, data) {
  let tr = $(rowjq).closest('tr');
  let row = table.row(tr);

  if (row.child.isShown()) {
    row.child.hide();
    // tr.find('a').first().html('<i class="plus circle icon"></i>');

    tr.removeClass('shown');
  } else {
    let s = '<div class="ui grid"><div class="row">';

    s += '<div class="wide column">';
    s += '<div class="ui relaxed divided list" style="white-space: normal;">';

    s += '<div class="item"><div class="content"><div class="header">Anzahl Transaktionen</div>' +
      '<div class="description">' + data.transactions + '</div></div></div>';

    if (data.exchangeId !== null) {
      let ex = _.head(_.filter(data.stock.exchanges, x => x.id == data.exchangeId));

      s += '<div class="item"><div class="content"><div class="header">Handelsplatz für berechnete Werte</div>' +
        '<div class="description">' + ex.name + '</div></div></div>';

      s += '<div class="item"><div class="content"><div class="header">Aktueller Kurs</div>' +
        '<div class="description">' + Util.formatAmount(data.value / data.units) + '</div></div></div>';

      if (data.previousValue !== null)
        s += '<div class="item"><div class="content"><div class="header">Voriger Kurs</div>' +
        '<div class="description">' + Util.formatAmount(data.previousValue / data.units) + '</div></div></div>';
    }

    s += '</div></div>';

    row.child(s).show();
    // tr.find('a').first().html('<i class="minus circle icon"></i>');

    tr.addClass('shown');
  }
};

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  $('#date', location).calendar({
    type: 'date',
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
    onChange: function(d) {
      date = moment(d);
      dateModified = true;
      refresh();
    }
  });

  table = $('#table-portfolio', location).DataTable({
    'paging': false,
    'ordering': true,
    'info': false,
    'dom': 'rtip',
    'data': [],
    'language': Util.datatableLanguage,
    createdRow: function(row, data) {
      $(row).on('click', () => showDetails(row, data));

      if (data.diff !== null)
        $(row).find('td:nth-child(7)').addClass(data.diff == 0 ? 'warning' : (data.diff < 0 ? 'negative' : 'positive'));

      if (data.diffDay != null)
        $(row).find('td:nth-child(6)').addClass(data.diffDay == 0.0 ? 'warning' : (data.diffDay < 0 ? 'negative' : 'positive'));

      if (data.stockIrr != null)
        $(row).find('td:nth-child(8)').addClass(data.stockIrr < 0.0 ? 'negative' : (data.stockIrr < 0.02 ? 'warning' : 'positive'));

      $('td:nth-child(3)', row).addClass('right aligned');
      $('td:nth-child(4)', row).addClass('right aligned');
      $('td:nth-child(5)', row).addClass('right aligned');
      $('td:nth-child(6)', row).addClass('right aligned');
      $('td:nth-child(7)', row).addClass('right aligned');
      $('td:nth-child(8)', row).addClass('right aligned');
    },
    columns: [{
      title: 'Wertpapier',
      data: 'stock.info.title'
    }, {
      title: 'ISIN',
      data: 'stock.isin',
      width: '10%'
    }, {
      title: 'Einheiten',
      data: 'units',
      width: '7%'

    }, {
      title: 'Kaufpreis',
      data: 'invested',
      width: '10%',
      render: function(data) {
        return Util.formatAmount(data);
      }
    }, {
      title: 'Wert',
      data: 'valueS',
      width: '13%'
    }, {
      title: 'Veränderung (Vortag)',
      data: 'diffDayS',
      width: '10%'
    }, {
      title: 'Veränderung (Gesamt)',
      data: 'diffS',
      width: '10%'
    }, {
      title: 'Zinsfuß',
      data: 'stockIrr',
      width: '8%',
      render: function(data) {
        if (data === null)
          return '';
        else
          return Util.formatPercentage(data) + ' p.a.';
      }
    }, ]
  });

  $('#table-portfolio', location).append('<tfoot></tfoot>');

  refresh();
}
