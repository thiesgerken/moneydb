'use strict';

import moment from 'moment/src/moment.js';
import page from './balances_modal.html';
import $ from '/jquery.js';
import * as Common from '/common.js';

export function init() {
  $('body').append(page);

  $('.ui.form', '#modal-balances').form({
    fields: {
      amount: {
        rules: [{
          type: 'empty',
          prompt: 'Betrag darf nicht leer sein'
        }, {
          type: 'isamount',
          prompt: 'Betrag muss eine Zahl sein'
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
          prompt: 'Kein Konto angegeben'
        }]
      }
    }
  });
}

export function reset() {
  Common.makeAccountDropdown($('#account', '#modal-balances'), false);

  $('.ui.form', '#modal-balances').form('clear');
  $('.ui.error.message', '#modal-balances').empty();

  $('#date', '#modal-balances').calendar({
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
    }
  }).calendar('set date', null);

  $('#time', '#modal-balances').calendar({
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
  }).calendar('set date', '00:00');
}

export function load(b) {
  reset();

  $('#account', '#modal-balances').parent().dropdown('set selected', b.accountId.toString()).dropdown('refresh');
  $('#amount', '#modal-balances').val(b.amount.toFixed(2).replace('.', ','));

  $('#date', '#modal-balances').calendar('set date', moment(b.date).toDate());
  $('#time', '#modal-balances').calendar('set date', moment(b.date).format('LT'));
}

export function save(w) {
  let b;

  if (w == undefined)
    b = {
      id: -1,
      ownerId: -1
    };
  else
    b = $.extend(true, {}, w); // avoid changing of w

  let snds = moment($('#time', '#modal-balances').calendar('get date')).diff(moment({
    month: 0,
    day: 1,
    hour: 0,
    minute: 0,
    seconds: 0,
    milliseconds: 0
  }));

  // somehow, the time of the date calendar is not 00:00 ...
  let d = moment(moment($('#date', '#modal-balances').calendar('get date')).format('L'), 'L');

  b.date = d.add(snds, 'milliseconds').toISOString();
  b.accountId = parseInt($('#account', '#modal-balances').parent().dropdown('get value'));
  b.amount = parseFloat($('#amount', '#modal-balances').val().replace(',', '.'));

  return b;
}
