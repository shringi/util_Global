$(document).ready(function() {
  var table = $('#example').DataTable( {
    initComplete: function () {
      this.api().columns().every( function () {
        var column = this;
        var select = $('<select><option value=""></option></select>')
          .appendTo( $(column.footer()).empty() )
          .on( 'change', function () {
            var val = $.fn.dataTable.util.escapeRegex(
              $(this).val()
            );

            column
              .search( val ? '^'+val+'$' : '', true, false )
              .draw();
          } );

        column.data().unique().sort().each( function ( d, j ) {
          select.append( '<option value="'+d+'">'+d+'</option>' );
        } );
      } );
    }
  } );
  
  table.on('draw', function () {
    table.columns().indexes().each( function ( idx ) {
      var select = $(table.column( idx ).footer()).find('select');
      
      if ( select.val() === '' ) {
        select
          .empty()
          .append('<option value=""/>');

        table.column(idx, {search:'applied'}).data().unique().sort().each( function ( d, j ) {
          select.append( '<option value="'+d+'">'+d+'</option>' );
        } );
      }
    } );
  } );
} );