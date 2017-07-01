$( document ).ready(function() {
    $('#go').click(function() {
        $.get("rhymes/" + encodeURI($("#verse").val()), function( result ) {
            $('#rhymes').val(result);
        });
        $.get("rhymes/strict/" + encodeURI($("#verse").val()), function( result ) {
            $('#rhymes_strict').val(result);
        });
    });
});
