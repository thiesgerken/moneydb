'use strict';

import page from './automation_modal.html';
import $ from '/jquery.js';
import * as Common from '/common.js';

export function init() {
  $('body').append(page);

  $('.ui.form', '#modal-automation').form({
    fields: {
      title: {
        rules: [{
          type: 'empty',
          prompt: 'Titel darf nicht leer sein'
        }]
      },
      'amount-filter': {
        rules: [{
          type: 'isamountorempty',
          prompt: 'Betrag muss eine Zahl oder leer sein'
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
      priority: {
        rules: [{
            type: 'empty',
            prompt: 'Priorität darf nicht leer sein'
          },
          {
            type: 'integer',
            prompt: 'Priorität muss eine Zahl sein'
          }
        ]
      }
    }
  });
}

export function reset() {
  Common.makeAccountDropdown($('#account-filter', '#modal-automation'), true, true);
  $('#account-filter', '#modal-automation').parent().dropdown('set selected', '-1').dropdown('refresh');

  $('.ui.form', '#modal-automation').form('clear');
  $('.ui.error.message', '#modal-automation').empty();
}

export function load(c) {
  reset();

  $('#time-filter', '#modal-automation').val(c.regexTime);
  $('#amount-filter', '#modal-automation').val(c.filterAmount ? c.filterAmount.toFixed(2) : '');
  $('#transaction-filter', '#modal-automation').val(c.regexTransaction);

  if (c.filterAccount)
    $('#account-filter', '#modal-automation').parent().dropdown('set selected', c.filterAccount.toString()).dropdown('refresh');
  else
    $('#account-filter', '#modal-automation').parent().dropdown('set selected', '-1').dropdown('refresh');

  $('#title', '#modal-automation').val(c.title);
  $('#template', '#modal-automation').val(c.templateId.toString());
  $('#priority', '#modal-automation').val(c.priority.toString());
}

export function save(w) {
  let c;

  if (w == undefined)
    c = {
      id: -1,
      lastDelivery: null,
      ownerId: -1
    };
  else
    c = $.extend(true, {}, w); // avoid changing of w

  c.title = $('#title', '#modal-automation').val();
  c.templateId = parseInt($('#template', '#modal-automation').val());
  c.priority = parseInt($('#priority', '#modal-automation').val());

  c.regexTime = $('#time-filter', '#modal-automation').val();
  if (c.regexTime == '') c.regexTime = null;

  c.filterAccount = parseInt($('#account-filter', '#modal-automation').parent().dropdown('get value'));
  if (c.filterAccount == -1) c.filterAccount = null;

  if ($('#amount-filter', '#modal-automation').val() == '')
    c.filterAmount = null;
  else
    c.filterAmount = parseFloat($('#amount-filter', '#modal-automation').val().replace(',', '.'));

  c.regexTransaction = $('#transaction-filter', '#modal-automation').val();
  if (c.regexTransaction == '') c.regexTransaction = null;

  return c;
}
