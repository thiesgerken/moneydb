'use strict';

import moment from 'moment/src/moment.js';
import page from './expenses_modal.html';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Common from '/common.js';

let sharingError;
let sharingState;

const initSharingModal = function() {
  initSharingRow($('.fields', '#modal-expenses-sharing').first());

  $('#add', '#modal-expenses-sharing').click(function() {
    addSharingRow();
  });
};

const addSharingRow = function() {
  let x = $('.fields', '#modal-expenses-sharing').first().clone();
  x.find('#remove', '#modal-expenses-sharing').show().click(function() {
    x.detach();
  });
  x.find('#add', '#modal-expenses-sharing').hide();
  initSharingRow(x);
  Common.makeAccountDropdown(x.find('#sharing-account', '#modal-expenses-sharing'));
  x.find('#sharing-account', '#modal-expenses-sharing').parent().dropdown();
  x.find('#sharing-type', '#modal-expenses-sharing').parent().dropdown();

  x.appendTo($('.fields', '#modal-expenses-sharing').first().parent());
  return x;
};

const initSharingRow = function(rjq) {
  rjq.find('#sharing-type', '#modal-expenses-sharing').change(function() {
    let v = $(this).val();

    // if (v == "equal") {
    //   rjq.find('#expsharingmodal_fixed').parent().parent().hide();
    //   rjq.find('#expsharingmodal_type').parent().parent().removeClass("three").removeClass("seven").removeClass("wide").addClass("seven").addClass("wide")
    // } else {
    rjq.find('#sharing-fixed', '#modal-expenses-sharing').parent().parent().show();
    rjq.find('#sharing-type', '#modal-expenses-sharing').parent().parent().removeClass('three').removeClass('seven').removeClass('wide').addClass('three').addClass('wide');

    let unit = '';

    switch (v) {
      case 'customamount':
        unit = '&euro;';
        break;
      case 'custompercentage':
        unit = '%';
        break;
    }

    rjq.find('#sharing-fixed', '#modal-expenses-sharing').parent().children().last().html(unit);

    if (v == 'equal')
      rjq.find('#sharing-fixed', '#modal-expenses-sharing').val('1');
    else
      rjq.find('#sharing-fixed', '#modal-expenses-sharing').val('');
  });

  rjq.find('#sharing-type', '#modal-expenses-sharing').parent().dropdown('set selected', 'equal').dropdown('refresh').trigger('change');
};

const loadSharingState = function() {
  for (let i = 0; i < sharingState.length; i++) {
    let x = (i == 0) ? $('.fields', '#modal-expenses-sharing').first() : addSharingRow();
    let si = sharingState[i];

    let tp;
    let acc;
    let val;

    if (si.sharingType == 'Equal') {
      acc = si.sharingAccountId;
      tp = 'equal';
      val = si.parameter.toFixed(0);
    } else if (si.sharingType == 'FixedAmount') {
      acc = si.sharingAccountId;
      tp = 'customamount';
      val = si.parameter.toFixed(2);
    } else if (si.sharingType == 'FixedFraction') {
      acc = si.sharingAccountId;
      tp = 'custompercentage';
      val = (100 * si.parameter).toFixed(2);
    } else {
      Util.errorMsg('Bad error! unknown sharing tag!', si);
    }

    x.find('#sharing-type').parent().dropdown('set selected', tp).dropdown('refresh');
    x.find('#sharing-type').trigger('change');
    x.find('#sharing-fixed').val(val);
    x.find('#sharing-account').parent().dropdown('set selected', acc.toString()).dropdown('refresh');
  }
};

let saveSharingState = function() {
  sharingState = [];
  sharingError = false;

  $('.fields', '#modal-expenses-sharing').each(function() {
    let tp = $(this).find('#sharing-type').val();

    let fac1 = $(this).find('#sharing-fixed').val().replace(',', '.');
    let acc1 = $(this).find('#sharing-account').val();

    let fac = parseFloat(fac1);
    let acc = parseInt(acc1);

    if (isNaN(acc) || !Util.isInt(acc1))
      sharingError = true;

    if (tp == 'equal') {
      if (!Util.isFloat(fac1) || (fac != -1 && fac != 1))
        sharingError = true;

      sharingState.push({
        calculatedAmount: 0,
        sharingType: 'Equal',
        sharingAccountId: acc,
        parameter: fac >= 0 ? 1 : -1
      });
    } else if (tp == 'custompercentage') {
      if (!Util.isFloat(fac1) || fac == 0)
        sharingError = true;

      sharingState.push({
        calculatedAmount: 0,
        sharingType: 'FixedFraction',
        sharingAccountId: acc,
        parameter: fac / 100
      });
    } else if (tp == 'customamount') {
      if (!Util.isFloat(fac1) || fac == 0)
        sharingError = true;

      sharingState.push({
        calculatedAmount: 0,
        sharingType: 'FixedAmount',
        sharingAccountId: acc,
        parameter: fac
      });
    } else
      sharingError = true;
  });
};

let resetSharingModal = function() {
  let x = $('.fields', '#modal-expenses-sharing').first().detach();
  $('.form', '#modal-expenses-sharing').empty();
  $('.form', '#modal-expenses-sharing').append(x);

  Common.makeAccountDropdown($('#sharing-account', '#modal-expenses-sharing'));
  $('#sharing-account', '#modal-expenses-sharing').parent().dropdown('clear').dropdown('refresh');
  $('#sharing-type', '#modal-expenses-sharing').val('equal').trigger('change').parent().dropdown('refresh');
  $('#sharing-fixed', '#modal-expenses-sharing').val('1');
};

export function init() {
  $('body').append(page);

  $.fn.form.settings.rules.sndaccount = function(value) {
    let t = $('#type', '#modal-expenses').parent().dropdown('get value');

    return t == 'normal' || t == 'sharedcustom' || value != '';
  };

  $.fn.form.settings.rules.bookingempty = function(value) {
    return !$('#booking', '#modal-expenses').prop('checked') || value != '';
  };

  $.fn.form.settings.rules.sharingopts = function(value) {
    return sharingState.length > 0 || value != 'sharedcustom';
  };

  $.fn.form.settings.rules.sharingoptserror = function() {
    let t = $('#type', '#modal-expenses').parent().dropdown('get value');

    return !sharingError || t != 'sharedcustom';
  };

  $('.ui.form', '#modal-expenses').form({
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
      type: {
        rules: [{
          type: 'empty',
          prompt: 'Kein Typ angegeben'
        }, {
          type: 'sharingopts',
          prompt: 'Typ ist "Gemeinsam", aber keine Einstellungen gemacht'
        }, {
          type: 'sharingoptserror',
          prompt: 'Typ ist "Gemeinsam", aber Einstellungen sind fehlerhaft'
        }]
      },
      sndaccount: {
        rules: [{
          type: 'sndaccount',
          prompt: 'Kein zweites Konto angegeben'
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
      'booking-date': {
        rules: [{
          type: 'bookingempty',
          prompt: 'Buchungsdatum darf nicht leer sein'
        }]
      },
      'booking-time': {
        rules: [{
          type: 'bookingempty',
          prompt: 'Buchungszeit darf nicht leer sein'
        }]
      },
      account: {
        rules: [{
          type: 'empty',
          prompt: 'Kein Konto angegeben'
        }]
      },
      category: {
        rules: [{
          type: 'empty',
          prompt: 'Keine Kategorie angegeben'
        }]
      }
    }
  });

  $('#booking', '#modal-expenses').change(function() {
    if (this.checked) {
      $('#booking-date .input', '#modal-expenses').removeClass('disabled');
      $('#booking-time .input', '#modal-expenses').removeClass('disabled');
    } else {
      $('#booking-date .input', '#modal-expenses').addClass('disabled');
      $('#booking-time .input', '#modal-expenses').addClass('disabled');
    }
  });

  $('#type', '#modal-expenses').change(function() {
    let v = $(this).val();

    if (v == 'normal') {
      $('#configure', '#modal-expenses').parent().hide();
      $('#sndaccount', '#modal-expenses').parent().parent().hide();
      $('#type', '#modal-expenses').parent().parent().removeClass('three').removeClass('eight').removeClass('wide').addClass('eight').addClass('wide');
    } else if (v == 'sharedcustom') {
      if (sharingState.length == 0)
        sharingState = [{
          'tag': 'Equal',
          'contents': Common.visibleAccounts[0]['id\'']
        }];

      $('#configure', '#modal-expenses').parent().show();
      $('#sndaccount', '#modal-expenses').parent().parent().hide();
      $('#type', '#modal-expenses').parent().parent().removeClass('three').removeClass('eight').removeClass('wide').addClass('three').addClass('wide');
    } else {
      $('#configure', '#modal-expenses').parent().hide();
      $('#sndaccount', '#modal-expenses').parent().parent().show();
      $('#type', '#modal-expenses').parent().parent().removeClass('three').removeClass('eight').removeClass('wide').addClass('three').addClass('wide');
    }
  });

  $('#configure', '#modal-expenses').click(function() {
    $('#modal-expenses-sharing').modal({
      observeChanges: true,
      allowMultiple: true,
      onApprove: function() {
        saveSharingState();
      }
    }).modal('show');
  });

  initSharingModal();
  resetSharingModal();
}

export function reset() {
  Common.makeCategoryDropdown($('#category', '#modal-expenses'));
  Common.makeAccountDropdown($('#account', '#modal-expenses'));
  Common.makeAccountDropdown($('#sndaccount', '#modal-expenses'));

  sharingState = [];
  sharingError = false;

  $('.ui.form', '#modal-expenses').form('clear');
  $('.ui.error.message', '#modal-expenses').empty();
  $('#type', '#modal-expenses').val('normal').trigger('change').parent().dropdown('refresh');

  $('#date', '#modal-expenses').calendar({
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
        $('#time', '#modal-expenses').calendar('set date', moment().format('LT'));
      else
        $('#time', '#modal-expenses').calendar('set date', '12:00');
    }
  }).calendar('set date', null);

  $('#time', '#modal-expenses').calendar({
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

  $('#booking-date', '#modal-expenses').calendar({
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
        $('#booking-time', '#modal-expenses').calendar('set date', moment().format('LT'));
      else
        $('#booking-time', '#modal-expenses').calendar('set date', '12:00');
    }
  }).calendar('set date', null);

  $('#booking-time', '#modal-expenses').calendar({
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

  $('#booking', '#modal-expenses').prop('checked', false);
  $('#booking', '#modal-expenses').trigger('change');

  resetSharingModal();
}

export function load(e) {
  reset();

  if (e.sharing.length == 0) {
    $('#type', '#modal-expenses').parent().dropdown('set selected', 'normal');
  } else if (e.sharing.length == 1 && e.sharing[0].sharingType == 'FixedFraction' && e.sharing[0].parameter == 1) {
    $('#type', '#modal-expenses').parent().dropdown('set selected', 'movement');
    $('#sndaccount', '#modal-expenses').parent().dropdown('set selected', e.sharing[0].sharingAccountId.toString()).dropdown('refresh');
  } else if (e.sharing.length == 1 && e.sharing[0].sharingType == 'Equal' && e.sharing[0].parameter == 1) {
    $('#type', '#modal-expenses').parent().dropdown('set selected', 'sharedequal');
    $('#sndaccount', '#modal-expenses').parent().dropdown('set selected', e.sharing[0].sharingAccountId.toString()).dropdown('refresh');
  } else {
    $('#type', '#modal-expenses').parent().dropdown('set selected', 'sharedcustom');
    sharingState = e.sharing;
  }

  $('#type', '#modal-expenses').parent().dropdown('refresh');
  $('#type', '#modal-expenses').trigger('change');
  $('#category', '#modal-expenses').parent().dropdown('set selected', e.categoryId.toString()).dropdown('refresh');
  $('#account', '#modal-expenses').parent().dropdown('set selected', e.accountId.toString()).dropdown('refresh');
  $('#amount', '#modal-expenses').val(e.amount.toFixed(2));
  $('#title', '#modal-expenses').val(e.title);
  $('#store', '#modal-expenses').val(e.store);
  $('#description', '#modal-expenses').val(e.description);
  $('#transaction', '#modal-expenses').val(e.transaction);
  $('#comments', '#modal-expenses').val(e.comments);
  $('#flags', '#modal-expenses').parent().dropdown('set exactly', e.flags).dropdown('refresh');

  $('#date', '#modal-expenses').calendar('set date', moment(e.valueDate).toDate());
  $('#time', '#modal-expenses').calendar('set date', moment(e.valueDate).format('LT'));

  if (e.bookingDate) {
    $('#booking', '#modal-expenses').prop('checked', true);
    $('#booking-date', '#modal-expenses').calendar('set date', moment(e.bookingDate).toDate());
    $('#booking-time', '#modal-expenses').calendar('set date', moment(e.bookingDate).format('LT'));
  } else {
    $('#booking', '#modal-expenses').prop('checked', false);
    $('#booking-date', '#modal-expenses').calendar('set date', '');
    $('#booking-time', '#modal-expenses').calendar('set date', '');
  }

  $('#booking', '#modal-expenses').trigger('change');

  resetSharingModal();
  loadSharingState();
}

export function save(w) {
  let e;

  if (w == undefined)
    e = {
      id: -1,
      lastModified: moment().toISOString(),
      ownerId: -1,
      creationDate: moment().toISOString(),
      lastModifiedBy: -1,
      effectiveAmount: 0
    };
  else
    e = $.extend(true, {}, w); // avoid changing of w

  e.lastModifiedThrough = 'moneydb-web';

  let tt = $('#type', '#modal-expenses').parent().dropdown('get value');

  if (tt == 'normal')
    e.sharing = [];
  else if (tt == 'movement')
    e.sharing = [{
      calculatedAmount: 0,
      parameter: 1,
      sharingAccountId: parseInt($('#sndaccount', '#modal-expenses').parent().dropdown('get value')),
      sharingType: 'FixedFraction'
    }];
  else if (tt == 'sharedequal')
    e.sharing = [{
      calculatedAmount: 0,
      parameter: 1,
      sharingAccountId: parseInt($('#sndaccount', '#modal-expenses').parent().dropdown('get value')),
      sharingType: 'Equal'
    }];
  else
    e.sharing = sharingState;

  let snds = moment($('#time', '#modal-expenses').calendar('get date')).diff(moment({
    month: 0,
    day: 1,
    hour: 0,
    minute: 0,
    seconds: 0,
    milliseconds: 0
  }));

  // somehow, the time of the date calendar is not 00:00 ...
  let d = moment(moment($('#date', '#modal-expenses').calendar('get date')).format('L'), 'L');

  e.valueDate = d.add(snds, 'milliseconds').toISOString();
  e.categoryId = parseInt($('#category', '#modal-expenses').parent().dropdown('get value'));
  e.accountId = parseInt($('#account', '#modal-expenses').parent().dropdown('get value'));
  e.amount = parseFloat($('#amount', '#modal-expenses').val().replace(',', '.'));
  e.title = $('#title', '#modal-expenses').val();
  e.store = $('#store', '#modal-expenses').val();
  e.description = $('#description', '#modal-expenses').val();
  e.transaction = $('#transaction', '#modal-expenses').val();
  e.comments = $('#comments', '#modal-expenses').val();

  if ($('#booking', '#modal-expenses').prop('checked')) {
    let snds2 = moment($('#booking-time', '#modal-expenses').calendar('get date')).diff(moment({
      month: 0,
      day: 1,
      hour: 0,
      minute: 0,
      seconds: 0,
      milliseconds: 0
    }));

    // somehow, the time of the date calendar is not 00:00 ...
    let d2 = moment(moment($('#booking-date', '#modal-expenses').calendar('get date')).format('L'), 'L');
    e.bookingDate = d2.add(snds2, 'milliseconds').toISOString();
  } else
    e.bookingDate = undefined;

  let flags = $('#flags', '#modal-expenses').parent().dropdown('get value');
  if (flags == '')
    e.flags = [];
  else
    e.flags = flags.split(',');

  return e;
}
