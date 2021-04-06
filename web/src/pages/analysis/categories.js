'use strict';

import moment from 'moment/src/moment.js';
import page from './categories.html';
import tinycolor from 'tinycolor2';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as c3 from 'c3';

export let chart;
export let location;
let overviewData = [];
let deviationDict = {};
let currentSign = -1;

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  chart = c3.generate({
    bindto: $('#chart', location)[0],
    data: {
      x: 'x',
      type: 'bar',
      order: (d1, d2) => deviationDict[d1.id] - deviationDict[d2.id],
      columns: []
    },
    axis: {
      y: {
        tick: {
          format: v => ((currentSign <= 0 ? -1 : 1) * v).toFixed(0) + ' €'
        }
      },
      x: {
        type: 'timeseries',
        tick: {
          format: x => moment.utc(x).format('MMM YY')
        }
      }
    },
    grid: {
      x: {
        show: false
      },
      y: {
        show: true
      }
    },
    size: {
      height: 320
      //, width: 480
    },
    tooltip: {
      format: {
        title: d => moment.utc(d).format('MMMM YYYY'),
        value: function(value) {
          if (Math.abs(value) < 1e-10) return undefined;

          return Util.formatAmount((currentSign <= 0 ? -1 : 1) * value);
        }
      }
    }
  });

  location.find('.checkbox').checkbox({
    onChecked: function() {
      let id = $(this).attr('id');

      let tmp = currentSign;

      switch (id) {
        case 'categories-all':
          currentSign = 0;
          break;
        case 'categories-onlyexpenses':
          currentSign = -1;
          break;
        case 'categories-onlyincome':
          currentSign = 1;
          break;
      }

      if (tmp != currentSign)
        refresh();
    }
  });

  currentSign = -1;
  $('#categories-onlyexpenses', location).parent().checkbox('check');

  $('#categories-showall', location).click(() => chart.show());
  $('#categories-hideall', location).click(() => chart.hide());
}

export function refresh(ovData) {
  if (typeof ovData !== 'undefined')
    overviewData = ovData;

  let xs = [];

  let groups = [
    []
  ];

  let dataDict = {};
  let colorDict = {};

  let length = overviewData.length;
  for (let i = overviewData.length - 1; i >= 0 && overviewData[i].expenseCount == 0; i--)
    length--;

  if (length > 1 && isNaN(overviewData[length - 1].moneyDelta)) length--;

  for (let i = length - 1; i >= 0; i--) {
    xs.push(moment(overviewData[i].endDate).toDate());

    let valsIncome = overviewData[i].statsIncome;
    let valsExps = overviewData[i].statsExps;

    if (currentSign >= 0)
      for (let j = 0; j < valsIncome.children.length; j++) {
        let title = valsIncome.children[j].title;
        if (typeof valsIncome.children[j].id == 'undefined')
          title = 'Rest';

        if (!(title in dataDict)) {
          dataDict[title] = Array(length).fill(0.0);

          if (typeof valsIncome.children[j].color == 'undefined')
            colorDict[title] = '#CCCCCC';
          else
            colorDict[title] = valsIncome.children[j].color.toString();
          // colorDict[title] = tinycolor(valsIncome.statChildren[j].statColor).saturate(20).toString();
        }

        dataDict[title][length - 1 - i] -= (currentSign > 0 ? -1 : 1) * valsIncome.children[j].amount;
      }

    if (currentSign <= 0)
      for (let j = 0; j < valsExps.children.length; j++) {
        let title = valsExps.children[j].title;
        if (valsExps.children[j].id == null)
          title = 'Rest';

        if (!(title in dataDict)) {
          dataDict[title] = Array(length).fill(0.0);

          if (valsExps.children[j].color == null)
            colorDict[title] = '#CCCCCC';
          else
            colorDict[title] = tinycolor(valsExps.children[j].color).saturate(20).toString();
        }

        dataDict[title][length - 1 - i] += valsExps.children[j].amount;
      }
  }

  deviationDict = {};

  for (let title in dataDict) {
    let dt = dataDict[title];

    let mean = 0.0;
    for (let i = 0; i < dt.length; i++)
      mean += dt[i];

    mean /= dt.length;

    if (Math.abs(mean) < 1e-2 || dt.length <= 1)
      deviationDict[title] = 1e10;
    else {
      let dev = 0.0;
      for (let i = 0; i < dt.length; i++)
        dev += (dt[i] - mean) * (dt[i] - mean);

      dev = Math.sqrt(dev / (dt.length - 1));

      deviationDict[title] = dev / mean;
    }
  }

  let columns = [];

  for (let title in dataDict) {
    dataDict[title].unshift(title);
    columns.push(dataDict[title]);

    groups[0].push(title);
  }

  columns.sort((d1, d2) => deviationDict[d1[0]] - deviationDict[d2[0]]);

  xs.unshift('x');
  columns.unshift(xs);

  chart.groups(groups);
  chart.load({
    x: 'x',
    columns: columns,
    colors: colorDict,
    unload: true
  });
}
