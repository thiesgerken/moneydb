'use strict';

import Table from '/table.js';
import moment from 'moment/src/moment.js';
import tinycolor from 'tinycolor2';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as AutomationModal from './automation_modal.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'rules/automation',
    location: location,
    defaultOrder: [
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: false,
    formatter: () => '',
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: function(row, data) {
      $(row).find('td:nth-child(5)').attr('style', '');

      if (data.account)
        Util.setBackgroundColor($(row).find('td:nth-child(4)'), data.account.color, false);
    },
    colorRowDark: function(rowjq, data) {
      if (data.filterAmount && data.filterAmount != 0)
        rowjq.find('td:nth-child(5)').attr('style', 'background-color:' + tinycolor(data.filterAmount <= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');
    },
    preprocess: function(row) {
      if (row.filterAccount)
        row.account = Common.findAccount(row.filterAccount);
      else
        row.account = null;

      return row;
    },
    createdRow: function(row, data) {
      if (data.filterAmount && data.filterAmount != 0)
        $(row).find('td:nth-child(5)').addClass(data.filterAmount <= 0 ? 'positive' : 'negative');
    },
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'title',
      title: 'Name der Regel',
      width: '16em'
    }, {
      data: 'templateId',
      title: 'ID der Vorlage',
      width: '8em'
    }, {
      data: 'filterAccount',
      title: 'Kontofilter',
      width: '10em',
      render: function(data, type, row) {
        if (row.account)
          return Common.accountToString(row.account);
        else return '(leer)';
      }
    }, {
      data: 'filterAmount',
      title: 'Betragsfilter',
      width: '10em',
      render: function(data, type, row) {
        if (row.filterAmount)
          return Util.formatAmount(row.filterAmount);
        else return '(leer)';
      }
    }, {
      data: 'regexTime',
      title: 'Zeitfilter',
      width: '9em',
      render: function(data, type, row) {
        if (row.regexTime)
          return row.regexTime;
        else return '(leer)';
      }
    }, {
      data: 'regexTransaction',
      title: 'Transaktionsfilter',
      render: function(data, type, row) {
        if (row.regexTransaction)
          return row.regexTransaction;
        else return '(leer)';
      }
    }, {
      data: 'lastDelivery',
      width: '8em',
      title: 'Zuletzt genutzt',
      render: function(data, type, row) {
        if (row.lastDelivery)
          return moment(row.lastDelivery).format('L');
        else return '(nie)';
      }
    }, {
      data: 'priority',
      title: 'Wichtigkeit',
      width: '5em'
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Automatische Ausgabe <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');

      $('#modal-delete').modal({
        onApprove: function() {
          Api.deleteRulesAutomationById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte automatische Ausgabe nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-automation .header').html('Automatische Ausgabe erstellen');
      AutomationModal.reset();

      $('#modal-automation').modal({
        observeChanges: true,
        allowMultiple: true,
        onApprove: function() {
          $('#modal-automation .ui.form').submit();
          if (!$('#modal-automation .ui.form').form('is valid'))
            return false;

          Api.postRulesAutomation(AutomationModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte automatische Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          AutomationModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selected, draw) {
      $('#modal-automation .header').html('Automatische Ausgabe bearbeiten');
      AutomationModal.load(selected);

      $('#modal-automation').modal({
        observeChanges: true,
        allowMultiple: true,
        onApprove: function() {
          $('#modal-automation .ui.form').submit();
          if (!$('#modal-automation .ui.form').form('is valid'))
            return false;

          Api.putRulesAutomationById(selected.id, AutomationModal.save(selected), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte automatische Ausgabe nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          AutomationModal.load(selected);
        }
      }).modal('show');
    },
    duplicateHandler: function(selected, draw) {
      $('#modal-automation .header').html('Automatische Ausgabe kopieren');
      AutomationModal.load(selected);

      $('#modal-automation').modal({
        observeChanges: true,
        allowMultiple: true,
        onApprove: function() {
          $('#modal-automation .ui.form').submit();
          if (!$('#modal-automation .ui.form').form('is valid'))
            return false;

          Api.postRulesAutomation(AutomationModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte automatische Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          AutomationModal.load(selected);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  AutomationModal.init();
}

export function refresh() {
  table.refresh();
}
