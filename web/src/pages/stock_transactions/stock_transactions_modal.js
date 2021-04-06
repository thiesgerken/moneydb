'use strict';

import page from './stock_transactions_modal.html';
import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Common from '/common.js';

export function init() {
  $('body').append(page);

  $('#modal-stock-transactions .ui.form').form({
    fields: {
      title: {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      amount: {
        rules: [{
          type: 'empty',
          prompt: 'Betrag darf nicht leer sein'
        }, {
          type: 'isamount',
          prompt: 'Betrag muss eine Zahl sein'
        }]
      },
      // exchange: {
      //   rules: [{
      //     type: 'empty',
      //     prompt: 'Handelsplatz darf nicht leer sein'
      //   }]
      // },
      units: {
        rules: [{
            type: 'empty',
            prompt: 'Einheiten dürfen nicht leer sein'
          }, {
            type: 'isamount',
            prompt: 'Einheiten müssen eine Zahl sein'
          }
          // ,{
          // type: 'nonzero',
          // prompt: 'Einheiten dürfen nicht null sein'
          // }
        ]
      },
      fees: {
        rules: [{
          type: 'empty',
          prompt: 'Gebühren dürfen nicht leer sein'
        }, {
          type: 'isamount',
          prompt: 'Gebühren müssen eine Zahl sein'
        }]
      },
      date: {
        rules: [{
          type: 'empty',
          prompt: 'Datum darf nicht leer sein'
        }]
      },
      time: {
        rules: [{
          type: 'empty',
          prompt: 'Zeit darf nicht leer sein'
        }]
      },
      account: {
        rules: [{
          type: 'empty',
          prompt: 'Kein Depot angegeben'
        }]
      },
      stock: {
        rules: [{
          type: 'empty',
          prompt: 'Kein Wertpapier angegeben'
        }]
      }
    }
  });

  $('#amount', '#modal-stock-transactions').change(function() {
    refresh_calculations();
  });

  $('#fees', '#modal-stock-transactions').change(function() {
    refresh_calculations();
  });

  $('#units', '#modal-stock-transactions').change(function() {
    refresh_calculations();
  });
}

function refresh_calculations() {
  let units = $('#units', '#modal-stock-transactions').val().replace(',', '.');
  let amount = $('#amount', '#modal-stock-transactions').val().replace(',', '.');
  let fees = $('#fees', '#modal-stock-transactions').val().replace(',', '.');

  if (Util.isFloat(units) && Util.isFloat(amount) && parseFloat(units) != 0) {
    $('#price', '#modal-stock-transactions').val((parseFloat(amount) / parseFloat(units)).toFixed(4).replace('.', ','));
  } else { $('#price', '#modal-stock-transactions').val(''); }

  if (Util.isFloat(fees) && Util.isFloat(amount)) {
    $('#total-amount', '#modal-stock-transactions').val(Util.formatAmount(parseFloat(amount) + parseFloat(fees), ''));
  } else { $('#total-amount', '#modal-stock-transactions').val(''); }
}

export function reset() {
  Common.getSecuritiesAccounts().then(() => {
    Common.makeSecuritiesAccountDropdown($('#account', '#modal-stock-transactions'));
    Common.makeStockDropdown($('#stock', '#modal-stock-transactions'));
  });

  $('#modal-stock-transactions .ui.form').form('clear');
  $('#modal-stock-transactions .ui.error.message').empty();

  $('#date', '#modal-stock-transactions').calendar({
    type: 'date',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      date: function(d) {
        return moment(d).format('L');
      }
    },
    parser: {
      date: function(d) {
        return moment(d, 'L').toDate();
      }
    },
    onChange: function(date) {
      if (moment().isSame(date, 'day'))
        $('#time', '#modal-stock-transactions').calendar('set date', moment().format('LT'));
      else
        $('#time', '#modal-stock-transactions').calendar('set date', '12:00');
    }
  }).calendar('set date', null);

  $('#time', '#modal-stock-transactions').calendar({
    type: 'time',
    ampm: false,
    formatter: {
      time: function(d) {
        return moment(d).format('LT');
      }
    },
    parser: {
      time: function(d) {
        return moment(d, 'LT').toDate();
      }
    }
  }).calendar('set date', '12:00');

  Common.getSecuritiesAccounts().then(saccs => {
    if (saccs.length == 1) {
      $('#account', '#modal-stock-transactions').val(saccs[0].id).trigger('change').parent().dropdown('refresh');
    }
  });

  refresh_calculations();
}

export function load(e) {
  reset();

  $('#stock', '#modal-stock-transactions').parent().dropdown('set selected', e.stockId.toString()).dropdown('refresh');
  $('#account', '#modal-stock-transactions').parent().dropdown('set selected', e.accountId.toString()).dropdown('refresh');
  $('#amount', '#modal-stock-transactions').val(e.amount.toFixed(2));
  $('#units', '#modal-stock-transactions').val(e.units.toFixed(4));
  $('#fees', '#modal-stock-transactions').val(e.fees.toFixed(2));
  $('#exchange', '#modal-stock-transactions').val(e.exchange);

  $('#date', '#modal-stock-transactions').calendar('set date', moment(e.date).toDate());
  $('#time', '#modal-stock-transactions').calendar('set date', moment(e.date).format('LT'));

  refresh_calculations();
}

export function save(w) {
  let a;

  if (w == undefined)
    a = {
      id: -1,
      ownerId: -1,
    };
  else
    a = $.extend(true, {}, w); // avoid changing of w

  // somehow, the time of the date calendar is not 00:00 ...
  let d = moment(moment($('#date', '#modal-stock-transactions').calendar('get date')).format('L'), 'L');

  let snds = moment($('#time', '#modal-stock-transactions').calendar('get date')).diff(moment({
    month: 0,
    day: 1,
    hour: 0,
    minute: 0,
    seconds: 0,
    milliseconds: 0
  }));

  a.date = d.add(snds, 'milliseconds').toISOString();
  a.stockId = parseInt($('#stock', '#modal-stock-transactions').parent().dropdown('get value'));
  a.accountId = parseInt($('#account', '#modal-stock-transactions').parent().dropdown('get value'));
  a.amount = parseFloat($('#amount', '#modal-stock-transactions').val().replace(',', '.'));
  a.fees = parseFloat($('#fees', '#modal-stock-transactions').val().replace(',', '.'));
  a.units = parseFloat($('#units', '#modal-stock-transactions').val().replace(',', '.'));
  a.exchange = $('#exchange', '#modal-stock-transactions').val();

  return a;
}
