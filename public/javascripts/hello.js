$( document ).ready(function() {
    $('#not_found').hide()
    $('#rhymes').hide()
    $('#searching').hide()

    $('#go').click(function() {
        findRhymes()
    });
    $('#input_verse').keypress(function(event) {
        if(event.which == '13'){
            findRhymes()
        } 
    });
});

function findRhymes() {
    $('#rhymes').hide("fast")
    $('#not_found').hide("fast")
    $('#searching').show("fast")
    $.get("rhymes/strict/" + encodeURI($("#input_verse").val()), function( result ) {
        if (result == "No se ha encontrado\n") {
            $.get("rhymes/" + encodeURI($("#input_verse").val()), function( result ) {
                $('#searching').hide("fast")
                if (result == "No se ha encontrado\n") {
                    $('#not_found').show("fast")
                }    
                else {
                    $('#rhymes').html(result) 
                    $('#rhymes').show("fast")
                }
            });
        }
        else {
            $('#searching').hide("fast")
            $('#rhymes').html(result) 
            $('#rhymes').show("fast")
        }
    });
}
