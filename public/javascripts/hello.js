$( document ).ready(function() {
    $('#go').click(function() {
        $.get("rhymes/" + encodeURI($("#verse").val()), function( result ) {
            $('#rhymes').html(result);
        });
        $.get("rhymes/strict/" + encodeURI($("#verse").val()), function( result ) {
            $('#rhymes_strict').html(result);
        });
    });
});
