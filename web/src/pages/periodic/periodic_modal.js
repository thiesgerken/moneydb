'use strict';

import moment from 'moment/src/moment.js';
import page from './periodic_modal.html';
import $ from '/jquery.js';

export function init() {
  $('body').append(page);

  $('.ui.form', '#modal-periodic').form({
    fields: {
      title: {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      period: {
        rules: [{
          type: 'empty',
          prompt: 'Intervall darf nicht leer sein'
        }]
      },
      template: {
        rules: [{
            type: 'empty',
            prompt: 'Vorlagen-ID darf nicht leer sein'
          },
          {
            type: 'integer',
            prompt: 'Vorlagen-ID muss eine Zahl sein'
          }
        ]
      },
      'amount-replacement': {
        rules: [{
          type: 'isamountorempty',
          prompt: 'Betrag muss eine Zahl oder leer sein'
        }]
      },
      date: {
        rules: [{
          type: 'empty',
          prompt: 'Datum darf nicht leer sein'
        }]
      }
    }
  });
}

export function reset() {
  $('.ui.form', '#modal-periodic').form('clear');
  $('.ui.error.message', '#modal-periodic').empty();

  $('#last-creation', '#modal-periodic').calendar({
    type: 'datetime',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      datetime: function(d) {
        return moment(d).format('L LT');
      }
    },
    parser: {
      datetime: function(d) {
        return moment(d, 'L LT').toDate();
      }
    }
  }).calendar('set date', moment().toDate());
}

export function load(c) {
  reset();

  $('#period', '#modal-periodic').parent().dropdown('set selected', c.period).dropdown('refresh');
  $('#title', '#modal-periodic').val(c.title);
  $('#title-replacement', '#modal-periodic').val(c.titleReplacement);

  if (c.amountReplacement != null)
    $('#amount-replacement', '#modal-periodic').val(c.amountReplacement.toFixed(2));

  $('#last-creation', '#modal-periodic').calendar('set date', moment(c.lastCreation).toDate());
  $('#template', '#modal-periodic').val(c.templateId.toString());

  $('#last-creation', '#modal-periodic').calendar('set date', moment(c.lastCreation).toDate());
}

export function save(w) {
  let c;

  if (w == undefined)
    c = {
      id: -1,
      ownerId: -1
    };
  else
    c = $.extend(true, {}, w); // avoid changing of w

  c.title = $('#title', '#modal-periodic').val();
  c.templateId = parseInt($('#template', '#modal-periodic').val());
  c.period = $('#period', '#modal-periodic').parent().dropdown('get value');

  c.titleReplacement = $('#title-replacement', '#modal-periodic').val();
  if (c.titleReplacement == '') c.titleReplacement = null;

  if ($('#amount-replacement', '#modal-periodic').val() == '')
    c.amountReplacement = null;
  else
    c.amountReplacement = parseFloat($('#amount-replacement', '#modal-periodic').val());

  c.lastCreation = moment($('#last-creation', '#modal-periodic').calendar('get date')).toISOString();

  return c;
}
