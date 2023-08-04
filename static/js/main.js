$(document).ready(function () {

    $('#home').click(function () {
        $('#content').load('{{"about/index.html" | absURL }}');
    });

    $('#bookslink').click(function () {
        $('#content').load('{{"books/index.html" | absURL }}');
    });

    $('#essayslink').click(function () {
        $('#content').load('{{"essays/index.html" | absURL }}');
    });

});