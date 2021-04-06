'use strict';

import moment from 'moment/src/moment.js';
import page from './calendar.html';
import $ from '/jquery.js';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';

export let location;

let monthModified = false;
let month = moment();

export function refresh() {
  if (!monthModified)
    month = moment();

  let startDate = moment(month).startOf('month');
  let endDate = moment(month).endOf('month');
  let byBooking = $('#type-booking', location).parent().checkbox('is checked');

  $('#date-month', location).calendar('set date', month.format('LL'), true, false);

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
      'accounts': [],
      'categories': [],
      'from': startDate.toISOString(),
      'to': endDate.toISOString()
    }
  };

  let row = null;
  let rows = [];

  Api.postExpensesRenderedQuery(expOpts).then(function(es) {
    es = _.map(es.data, Util.flattenRecord);

    for (let day = moment(startDate); day.isBefore(endDate); day.add(1, 'days')) {
      if (!row || day.weekday() == 0) {
        row = $('<div>').addClass('row').css('padding', '0.5em');

        rows.push(row);

        for (let i = 0; i < day.weekday(); i++)
          row.append($('<div>').addClass('column'));
      }

      let expenses = _.filter(es, e => {
        if (byBooking && e.bookingDate != null)
          return moment(e.bookingDate).isSame(day, 'day');
        else
          return moment(e.valueDate).isSame(day, 'day');
      });

      let perCat = _.groupBy(expenses, e => e.categoryId);
      let totalCat = _.map(perCat, (c, k) => ({ id: k, total: _.sumBy(c, e => e.effectiveAmount) }));
      totalCat = _.sortBy(totalCat, x => -Math.abs(x.total));
      totalCat = _.filter(totalCat, x => x.total != 0);

      let box = $('<div>').addClass('column');
      let card = $('<div>').addClass('ui card');

      // date 
      let header = $('<div>').addClass('content').append($('<div>').addClass('right floated meta').html(day.format('dd, DD.MM')));
      card.append(header);

      // stripe
      let content = $('<div>').addClass('content').css('height', '0.65em');
      let tooltip = [];

      let colors = [];
      let weights = [];
      let sums = [0];

      for (let j = 0; j < totalCat.length; j++) {
        let w = Math.abs(totalCat[j].total);
        let cat = Common.findCategory(totalCat[j].id);

        colors.push(cat.color);
        weights.push(w);
        sums.push(sums[sums.length - 1] + weights[weights.length - 1]);
        tooltip.push(cat.title + ': ' + Util.formatAmount(totalCat[j].total, '€'));
      }

      let totalWeight = weights.reduce((a, b) => a + b, 0);

      if (totalWeight != 0) {
        let s = 'linear-gradient(to right, ';

        for (let i = 0; i < colors.length; i++) {
          if (i != 0)
            s += ',';

          s += colors[i];

          if (i != 0)
            s += ' ' + (sums[i] / totalWeight * 100).toFixed(0) + '%';

          if (i != colors.length - 1)
            s += ', ' + colors[i] + ' ' + (sums[i + 1] / totalWeight * 100).toFixed(0) + '%';
        }

        if (colors.length == 1)
          s += ', ' + colors[0];

        content.css('border-style', 'none');
        content.css('background-image', s + ')');
      }

      card.append(content);

      // total 
      let totalAmount = _.sumBy(expenses, e => e.effectiveAmount);
      let cls = '';

      if (totalAmount > 0) cls = 'red';
      else if (totalAmount < 0) cls = 'green';

      let label = $('<span>').addClass('ui ' + cls + ' label')
        .html(Util.formatAmount(totalAmount));

      if (tooltip.length > 0)
        label.attr('data-tooltip', tooltip.join(', ')).attr('data-position', 'right center');

      card.append($('<div>').addClass('extra content')
        .append($('<span>').addClass('right floated')
          .append(label)));

      // header.append($('<div>').addClass('left floated meta').append(label));

      box.append(card);
      row.append(box);
    }

    $('#grid-month', location).empty();
    $('#grid-month', location).append(rows);
  });
}

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  $('#date-month', location).calendar({
    type: 'month',
    today: true,
    ampm: false,
    firstDayOfWeek: 1,
    formatter: {
      datetime: function(d) {
        return moment(d).format('MMMM Y');
      }
    },
    parser: {
      datetime: function(d) {
        return moment(d, 'MMMM Y').toDate();
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
      month = moment(date);
      monthModified = true;
      refresh();
    }
  });

  $('#date-type', location).find('.checkbox').checkbox({
    onChecked: function() {
      refresh();
    }
  });


  $('#button-prev', location).click(() => {
    monthModified = true;
    month.subtract('1', 'months');

    refresh();
  });

  $('#button-next', location).click(() => {
    monthModified = true;
    month.add('1', 'months');

    refresh();
  });

  refresh();
}
