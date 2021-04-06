'use strict';

import Table from '/table.js';
import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    location: location,
    url: 'devices/rendered',
    defaultOrder: [
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: true,
    formatter: function(data) {
      let s = '<div class="ui relaxed divided list">' +
        '<div class="item"><div class="content"><div class="header">Token</div>' +
        '<div class="description">' + data.token + '</div></div></div>';

      s += '<div class="item"><div class="content"><div class="header">Benachrichtigt über</div>';
      s += '<div class="description"><ul class="ui list">';

      for (let i = 0; i < data.filters.length; i++) {
        s += '<li>';

        let x = data.filters[i];

        if (!x.onlyThrough && !x.onlySomeoneElse && !x.onlyNew)
          s += 'alle Ausgaben';
        else {

          if (x.onlyNew)
            s += 'neue Ausgaben';
          else
            s += 'Ausgaben';

          if (x.onlySomeoneElse || x.onlyThrough) s += ', die';
          if (x.onlySomeoneElse) s += ' durch jemand anderes';
          if (x.onlyThrough) s += ' via <span style="font-family:monospace; font-size:0.79em">' + x.onlyThrough + '</span>';
          if (x.onlySomeoneElse || x.onlyThrough) s += ' bearbeitet worden sind';
        }

        s += '</li>';
      }

      s += '</ul>';
      s += '</div></div></div>';
      return s + '</div>';
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    showRefreshButton: true,
    showCopyButton: false,
    showEditButton: false,
    showNewButton: false,
    showDeleteButton: true,
    colorRow: function() {},
    colorRowDark: function() {},
    createdRow: function(row) {
      $(row).find('td').last().html('');
    },
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'model',
      title: 'Modell',
      width: '16em'
    }, {
      data: 'firstContact',
      title: 'Erster Kontakt',
      width: '12em',
      render: function(data) {
        if (data == null) return '(nie)';
        return moment(data).format('L LT');
      }
    }, {
      data: 'lastContact',
      title: 'Letzter Kontakt',
      width: '12em',
      render: function(data) {
        if (data == null) return '(nie)';
        return moment(data).format('L LT');
      }
    }, {
      data: 'lastNotification',
      title: 'Letzte Benachrichtigung',
      width: '12em',
      render: function(data) {
        if (data == null) return '(nie)';
        return moment(data).format('L LT');
      }
    }, {
      data: 'notificationCount',
      title: 'Anzahl Benachrichtigungen',
      width: '14em'
    }, {
      data: 'model',
      title: ''
    }],
    refreshHandler: draw => draw(),
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Gerät <b>' + item.itemId + '</b> (<b>' + item.model + '</b>) wirklich löschen? Dieses Gerät wird dann nicht mehr über Änderungen informiert!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteDevicesRenderedById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Gerät nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function() {},
    modifyHandler: function() {},
    duplicateHandler: function() {}
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;
}

export function refresh() {
  table.refresh();
}
