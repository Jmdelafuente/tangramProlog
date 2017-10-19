
// function disableButtons(ask, next, stop, abort) {
//     $("#ask-btn").prop("disabled", ask);
//     $("#next-btn").prop("disabled", next);
//     $("#stop-btn").prop("disabled", stop);
//     $("#abort-btn").prop("disabled", abort);
// }


$(document).ready(function() {
    $("#sample-queries").on("change", function() {
        $("#queryid").val($("#sample-queries option:selected").text());
    });
    // $("#next-btn").on("click", next);
    // $("#stop-btn").on("click", stop);
    // $("#abort-btn").on("click", abort);
    $("#clear-btn").on("click", function() {
        $('#output').html('');
    });
});

