'use strict';

import moment from 'moment/src/moment.js';
import page from './history.html';
import $ from '/jquery.js';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as c3 from 'c3';

export let location;
export let absolute;
export let ppm;
export let startDate;
export let endDate;

let chart;
let data;
let overviewData;

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  startDate = moment().subtract(1, 'year');
  endDate = moment();

  $('#range-from', location).calendar({
    type: 'date',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      date: function(d) {
        return moment(d).format('LL');
      }
    },
    parser: {
      date: function(d) {
        return moment(d, 'LL').toDate();
      }
    },
    text: {
      days: ['S', 'M', 'D', 'M', 'D', 'F', 'S'],
      months: ['Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'],
      monthsShort: ['Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'],
      today: 'Heute',
      now: 'Jetzt'
    },
    onChange: function(date) {
      startDate = moment(moment(date).format('L'), 'L');
      refresh();
    }
  }).calendar('set date', startDate.toDate(), true, false);

  $('#range-to', location).calendar({
    type: 'date',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      date: function(d) {
        return moment(d).format('LL');
      }
    },
    parser: {
      date: function(d) {
        return moment(d, 'LL').toDate();
      }
    },
    text: {
      days: ['S', 'M', 'D', 'M', 'D', 'F', 'S'],
      months: ['Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'],
      monthsShort: ['Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'],
      today: 'Heute',
      now: 'Jetzt'
    },
    onChange: function(date) {
      endDate = moment(moment(date).format('L'), 'L');
      refresh();
    }
  }).calendar('set date', endDate.toDate(), true, false);

  chart = c3.generate({
    bindto: $('#chart', location)[0],
    data: {
      x: 'x',
      columns: []
    },
    size: {
      height: 520
      //, width: 480
    },
    axis: {
      y: {
        tick: {
          format: function(value) {
            return value.toFixed(0) + ' €';
          }
        }
      },
      x: {
        type: 'timeseries',
        tick: {
          format: function(value) {
            return moment(value).format('L');
          }
        }
      }
    },
    grid: {
      x: {
        show: false
      },
      y: {
        show: false
      }
    },
    tooltip: {
      format: {
        title: function(d) {
          if (absolute)
            return moment(d).format('LL LT');
          else {
            let period = moment(d).diff(moment(d).subtract(1, 'months')) / ppm;
            let v2 = moment(d).subtract(period, 'milliseconds');

            return v2.format('L LT') + ' bis ' + moment(d).format('L LT');
          }
        },
        value: function(value) {
          return Util.formatAmount(value);
        }
      }
    }
  });

  $('#show-all', location).click(() => chart.show());
  $('#hide-all', location).click(() => chart.hide());

  $('#interval', location).find('.checkbox').checkbox({
    onChecked: function() {
      refresh();
    }
  });

  $('#display-type', location).find('.checkbox').checkbox({
    onChecked: function() {
      refresh();
    }
  });

  $('#range', location).find('.checkbox').checkbox({
    onChecked: function() {
      refresh();

      if (!$('#range-custom', location).parent().checkbox('is checked'))
        $('#range .input', location).addClass('disabled');
      else
        $('#range .input', location).removeClass('disabled');
    }
  });

  $('#group-by', location).find('.checkbox').checkbox({
    onChecked: function() {
      // reuse old data (or do nothing)
      if (typeof data != 'undefined')
        updateChart(data);
    }
  });

  $('#range-firstbalance', location).parent().checkbox('set checked');
  $('#absolute', location).parent().checkbox('set checked');
  $('#group-by-availability', location).parent().checkbox('set checked');
  $('#range .input', location).addClass('disabled');
  $('#interval-monthly', location).parent().checkbox('set checked');
}

const updateChart = function(d) {
  data = d;

  let byAvail = $('#group-by-availability', location).parent().checkbox('is checked');
  let showAbsoluteValues = $('#absolute', location).parent().checkbox('is checked');
  let columns;
  let colorDict;

  if (byAvail) {
    colorDict = {};
    columns = [
      ['x'],
      ['Sofort'],
      ['≤ Wochen'],
      ['≤ Monate'],
      ['≤ Jahre'],
      ['≤ Jahrzehnte']
    ];

    if (Array.isArray(data)) {
      // monthly data from overview table
      for (let i = data.length - (showAbsoluteValues ? 1 : 2); i >= 0; i--) {
        columns[0].push(moment(data[i].endDate).toDate());

        for (let j = 1; j < columns.length; j++)
          if (showAbsoluteValues)
            columns[j].push(data[i].endBalances.total[j - 1].amount);
          else
            columns[j].push(data[i].endBalances.total[j - 1].amount - data[i + 1].endBalances.total[j - 1].amount);
      }
    } else {
      // requested data
      for (let i = data.dates.length - (showAbsoluteValues ? 1 : 2); i >= 0; i--) {
        columns[0].push(moment(data.dates[i]).toDate());

        for (let j = 1; j < columns.length; j++)
          if (showAbsoluteValues)
            columns[j].push(data.byAvailability[j - 1].data[i]);
          else
            columns[j].push(data.byAvailability[j - 1].data[i] - data.byAvailability[j - 1].data[i + 1]);
      }
    }
  } else {
    colorDict = {};
    columns = [];

    if (Array.isArray(data)) {
      _.forEach(data[0].endBalances.balances, function(v) {
        let acc = Common.findAccount(v.accountId);
        colorDict[acc.title] = acc.color; // tinycolor(acc.color).saturate(20).toString();
        columns.push([acc.title]);
      });

      columns.unshift(['x']);

      // monthly data from overview table
      for (let i = data.length - (showAbsoluteValues ? 1 : 2); i >= 0; i--) {
        columns[0].push(moment(data[i].endDate).toDate());

        for (let j = 1; j < columns.length; j++)
          if (showAbsoluteValues)
            columns[j].push(data[i].endBalances.balances[j - 1].amount);
          else
            columns[j].push(data[i].endBalances.balances[j - 1].amount - data[i + 1].endBalances.balances[j - 1].amount);

      }
    } else {
      _.forEach(data.byAccount, function(v) {
        let acc = Common.findAccount(v.id);
        colorDict[acc.title] = acc.color; // tinycolor(acc.color).saturate(20).toString();
        columns.push([acc.title]);
      });

      columns.unshift(['x']);

      for (let i = data.dates.length - (showAbsoluteValues ? 1 : 2); i >= 0; i--) {
        columns[0].push(moment(data.dates[i]).toDate());

        for (let j = 1; j < columns.length; j++)
          if (showAbsoluteValues)
            columns[j].push(data.byAccount[j - 1].data[i]);
          else
            columns[j].push(data.byAccount[j - 1].data[i] - data.byAccount[j - 1].data[i + 1]);
      }
    }
  }

  let firstLoad = chart.data().length == 0;

  absolute = showAbsoluteValues;

  chart.load({
    x: 'x',
    columns: columns,
    unload: true,
    done: function() {
      if (firstLoad && byAvail)
        chart.hide(['≤ Wochen', '≤ Monate', '≤ Jahre']);
    },
    // type: showAbsoluteValues ? (columns[0].length > 100 ? 'area' : 'area-spline') : 'bar',
    type: columns[0].length > 100 ? 'area' : 'area-spline',
    colors: colorDict,
    groups: []
  });
};

export function refresh(ovData) {
  if (typeof ovData !== 'undefined')
    overviewData = ovData;

  if (typeof overviewData !== undefined && $('#interval-monthly', location).parent().checkbox('is checked')) {
    ppm = 1;

    if ($('#range-firstexpense', location).parent().checkbox('is checked')) {
      let length = overviewData.length;
      for (let i = overviewData.length - 1; i >= 0 && overviewData[i].expenseCount == 0; i--)
        length--;

      updateChart(_.take(overviewData, length));
    } else if ($('#range-firstbalance', location).parent().checkbox('is checked')) {
      updateChart(overviewData);
    } else {
      updateChart(_.filter(overviewData, x => moment(x.endDate).isBetween(startDate, endDate, 'day', '[]')));
    }
  } else {
    let sD = startDate;
    let eD = endDate;

    if (!$('#range-custom', location).parent().checkbox('is checked') && typeof overviewData !== 'undefined') {
      eD = moment(overviewData[0].endDate);
      let length = overviewData.length;

      if ($('#range-firstexpense', location).parent().checkbox('is checked')) {
        for (let i = overviewData.length - 1; i >= 0 && overviewData[i].expenseCount == 0; i--)
          length--;
      }

      sD = moment(overviewData[length - 1].endDate);
    }

    let ppm_ = 1;
    let id = $('#interval', location).find('.checked').children().attr('id');
    switch (id) {
      case 'interval-monthly': // should use ovData, but okay ...
        ppm_ = 1;
        break;
      case 'interval-twoweekly':
        ppm_ = 2;
        break;
      case 'interval-weekly':
        ppm_ = 4;
        break;
      case 'interval-biweekly':
        ppm_ = 8;
        break;
      case 'interval-daily':
        ppm_ = 30;
        break;
      default:
        Util.errorMsg('Invalid interval, id=' + id);
    }

    Api.getComputeSimulations('Both', sD.toISOString(), eD.toISOString(), ppm_, function(e) {
      ppm = ppm_;
      updateChart(e);
    }, function(e) {
      Util.errorMsg('Anfragen der berechneten Kontostände schlug fehl.', e);
    });
  }
}
