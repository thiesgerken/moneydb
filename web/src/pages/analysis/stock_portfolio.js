'use strict';

import moment from 'moment/src/moment.js';
import page from './stock_portfolio.html';
import $ from '/jquery.js';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';

export let location;
let table;

let refreshing = false;

export function refresh(refresh_data) {
  refreshing = true;

  if (refresh_data === undefined)
    refresh_data = false;

  $.when(Common.getStocks(), Api.getComputeStocksPortfolio(refresh_data)).then((_s, x) => {
    x = x[0];

    let invested = 0.0;
    let value = 0.0;
    let hValue = 0.0;
    let diff = 0.0;
    let diffDay = 0.0;

    let stocks = _.map(x.stocks, y => {
      y.stock = Common.findStock(y.stockId);
      invested += y.invested;

      if (y.realtimeValue !== null && y.historicValue !== null && y.historicValue[0] != 0.0) {
        let perc = (y.realtimeValue[0] - y.historicValue[0]) / y.historicValue[0];
        y.diffDay = y.realtimeValue[0] - y.historicValue[0];
        y.diffDayS = Util.formatAmount(y.realtimeValue[0] - y.historicValue[0]) + ' (' + Util.formatPercentage(perc) + ')';

        if (diffDay !== null) diffDay += y.diffDay;
      } else {
        y.diffDay = null;
        y.diffDayS = null;
        diffDay = null;
      }

      if (y.realtimeValue !== null && y.invested != 0.0) {
        let perc = (y.realtimeValue[0] - y.invested) / y.invested;
        y.diff = y.realtimeValue[0] - y.invested;
        y.diffS = Util.formatAmount(y.realtimeValue[0] - y.invested) + ' (' + Util.formatPercentage(perc) + ')';
        if (diff !== null) diff += y.diff;
      } else {
        y.diff = null;
        y.diffS = null;
        diff = null;
      }

      if (y.realtimeValue !== null) {
        let ex = _.head(_.filter(y.stock.exchanges, x => x.id == y.realtimeValue[2]));

        y.value = y.realtimeValue[0];
        y.valueS = Util.formatAmount(y.value) + '<br />(' + ex.name + ', ' + moment(y.realtimeValue[1]).format('L LT') + ')';

        if (value !== null) value += y.value;
      } else {
        y.valueS = null;
        y.value = null;
        value = null;
      }

      if (y.historicValue !== null) {
        let ex = _.head(_.filter(y.stock.exchanges, x => x.id == y.historicValue[2]));

        y.hValue = y.historicValue[0];
        y.hValueS = Util.formatAmount(y.hValue) + '<br />(' + ex.name + ', ' + moment(y.historicValue[1]).format('L') + ')';

        if (hValue !== null) hValue += y.hValue;
      } else {
        y.hValueS = null;
        y.hValue = null;
        hValue = null;
      }

      return y;
    });

    table.clear().rows.add(stocks).draw();

    let valueS = '';
    if (value !== null) {
      valueS = Util.formatAmount(value);
    }

    let hValueS = '';
    if (hValue !== null) {
      hValueS = Util.formatAmount(hValue);
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
    if (x.portfolioRealtimeIrr !== null) {
      irrS = Util.formatPercentage(x.portfolioRealtimeIrr) + ' p.a.';
    }

    let irrHS = '';
    if (x.portfolioHistoricIrr !== null) {
      irrHS = Util.formatPercentage(x.portfolioHistoricIrr) + ' p.a.';
    }

    $('#table-portfolio tfoot', location).html('<tr><th>Gesamt</th><th></th><th></th><th class="right aligned">' + investedS + '</th><th class="right aligned">' + valueS + '</th><th class="right aligned">' + hValueS + '</th><th class="right aligned">' + diffDayS + '</th><th class="right aligned">' + diffS + '</th><th class="right aligned">' + irrS + '</th><th class="right aligned">' + irrHS + '</th></tr>');

    $('#button-refresh', location).addClass('icon').addClass('labeled').removeClass('loading');
    refreshing = false;
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

    // s += '<div class="wide column">';
    // s += '<div class="ui relaxed divided list" style="white-space: normal;">';

    // s += '<div class="item"><div class="content"><div class="header">Anzahl Transaktionen</div>' +
    //   '<div class="description">' + data.transactions + '</div></div></div>';

    // if (data.exchangeId !== null) {
    //   let ex = _.head(_.filter(data.stock.exchanges, x => x.id == data.exchangeId));

    //   s += '<div class="item"><div class="content"><div class="header">Handelsplatz für berechnete Werte</div>' +
    //     '<div class="description">' + ex.name + '</div></div></div>';

    //   s += '<div class="item"><div class="content"><div class="header">Aktueller Kurs</div>' +
    //     '<div class="description">' + Util.formatAmount(data.value / data.units) + '</div></div></div>';

    //   if (data.previousValue !== null)
    //     s += '<div class="item"><div class="content"><div class="header">Voriger Kurs</div>' +
    //     '<div class="description">' + Util.formatAmount(data.previousValue / data.units) + '</div></div></div>';
    // }

    s += '</div></div>';

    row.child(s).show();
    // tr.find('a').first().html('<i class="minus circle icon"></i>');

    tr.addClass('shown');
  }
};

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

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
        $(row).find('td:nth-child(8)').addClass(data.diff == 0 ? 'warning' : (data.diff < 0 ? 'negative' : 'positive'));

      if (data.diffDay != null)
        $(row).find('td:nth-child(7)').addClass(data.diffDay == 0.0 ? 'warning' : (data.diffDay < 0 ? 'negative' : 'positive'));

      if (data.realtimeIrr != null)
        $(row).find('td:nth-child(9)').addClass(data.realtimeIrr < 0.0 ? 'negative' : (data.realtimeIrr < 0.02 ? 'warning' : 'positive'));

      if (data.historicIrr != null)
        $(row).find('td:nth-child(10)').addClass(data.historicIrr < 0.0 ? 'negative' : (data.historicIrr < 0.02 ? 'warning' : 'positive'));

      $('td:nth-child(3)', row).addClass('right aligned');
      $('td:nth-child(4)', row).addClass('right aligned');
      $('td:nth-child(5)', row).addClass('right aligned');
      $('td:nth-child(6)', row).addClass('right aligned');
      $('td:nth-child(7)', row).addClass('right aligned');
      $('td:nth-child(8)', row).addClass('right aligned');
      $('td:nth-child(9)', row).addClass('right aligned');
      $('td:nth-child(10)', row).addClass('right aligned');
    },
    columns: [{
        title: 'Wertpapier',
        data: 'stock.info',
        render: data => data.title + '&nbsp;<a alt="auf onvista ansehen" target="_blank" href="https://www.onvista.de' + data.onvistaUrl + '"><i class="external icon"></i></a>'
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
        render: data => Util.formatAmount(data)
      },
      {
        title: 'Wert (Realtime)',
        data: 'valueS',
        width: '10%'
      },
      {
        title: 'Wert (letzter Handelstag)',
        data: 'hValueS',
        width: '10%'
      },
      {
        title: 'Veränderung (Intraday)',
        data: 'diffDayS',
        width: '10%'
      },
      {
        title: 'Veränderung (Gesamt)',
        data: 'diffS',
        width: '10%'
      },
      {
        title: 'Zinsfuß (Realtime)',
        data: 'realtimeIrr',
        width: '8%',
        render: data => {
          if (data === null)
            return '';
          else
            return Util.formatPercentage(data) + ' p.a.';
        }
      },
      {
        title: 'Zinsfuß (letzter Handelstag)',
        data: 'historicIrr',
        width: '8%',
        render: function(data) {
          if (data === null)
            return '';
          else
            return Util.formatPercentage(data) + ' p.a.';
        }
      },
    ]
  });

  $('#button-refresh', location).click(() => {
    if (!refreshing) {
      $('#button-refresh', location).removeClass('labeled').removeClass('icon').addClass('loading');
      refresh(true);
    }
  });

  $('#table-portfolio', location).append('<tfoot></tfoot>');

  refresh();
}
