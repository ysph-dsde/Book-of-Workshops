/**
 * This script creates a popup tooltip for text and code terms when hovered over.
 * It identifies elements with specific classes and attaches a popup displaying 
 * additional information based on their data attributes.
 * 
 * It includes the following features:
 *    1. Creating and appending popup elements.
 *    2. Displaying the popup on mouse enter.
 *    3. Hiding the popup on mouse leave.
 *
 * Author: Shelby Golden, M.S.
 *   Date: October 2025
 * 
 * Note: Annotated with the assistance of Yale's AI, Clarity.
 */

document.addEventListener('DOMContentLoaded', () => {
    /**
     * Creates a popup element and appends it to the target span element.
     * Sets up event listeners to show and hide the popup on mouse enter and leave.
     * 
     * @param {HTMLElement} span - The span element to attach the popup to.
     * @param {string} popupClass - The class name to assign to the created popup element.
     */
    function createPopup(span, popupClass) {
        // Create a div element to serve as the popup container for better structure than span
        const popup = document.createElement('div');
        
        // Assign the specified class to the popup element for styling
        popup.className = popupClass;
        
        // Use innerHTML to set the content of the popup from the data-hover-text attribute of the span
        popup.innerHTML = span.getAttribute('data-hover-text');
        
        // Append the created popup element to the span
        span.appendChild(popup);

        // Show the popup when the mouse enters the span
        span.addEventListener('mouseenter', () => {
            popup.style.display = 'block';
        });

        // Hide the popup when the mouse leaves the span
        span.addEventListener('mouseleave', () => {
            popup.style.display = 'none';
        });
    }

    // Select all elements with the class 'text-term' and create a popup for each
    document.querySelectorAll('.text-term').forEach(span => {
        createPopup(span, 'popup-text-term');
    });

    // Select all elements with the class 'code-term' and create a popup for each
    document.querySelectorAll('.code-term').forEach(span => {
        createPopup(span, 'popup-code-term');
    });
});