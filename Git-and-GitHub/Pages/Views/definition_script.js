document.addEventListener('DOMContentLoaded', () => {
    function createPopup(span, popupClass) {
        const popup = document.createElement('div'); // Use div for better structure than span
        popup.className = popupClass;
        popup.innerHTML = span.getAttribute('data-hover-text'); // Use innerHTML to parse HTML content
        span.appendChild(popup);

        span.addEventListener('mouseenter', () => {
            popup.style.display = 'block';
        });

        span.addEventListener('mouseleave', () => {
            popup.style.display = 'none';
        });
    }

    document.querySelectorAll('.text-term').forEach(span => {
        createPopup(span, 'popup-text-term');
    });

    document.querySelectorAll('.code-term').forEach(span => {
        createPopup(span, 'popup-code-term');
    });
});