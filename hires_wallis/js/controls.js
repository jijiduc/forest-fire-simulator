// Controls Module for Forest Fire Simulation Viewer

export class Controls {
    constructor(elements, viewer) {
        this.elements = elements;
        this.viewer = viewer;
        
        this.setupDragAndDrop();
    }
    
    setupDragAndDrop() {
        const dropZone = document.body;
        
        // Prevent default drag behaviors
        ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
            dropZone.addEventListener(eventName, this.preventDefaults, false);
        });
        
        // Highlight drop zone when item is dragged over it
        ['dragenter', 'dragover'].forEach(eventName => {
            dropZone.addEventListener(eventName, () => this.highlight(), false);
        });
        
        ['dragleave', 'drop'].forEach(eventName => {
            dropZone.addEventListener(eventName, () => this.unhighlight(), false);
        });
        
        // Handle dropped files
        dropZone.addEventListener('drop', (e) => this.handleDrop(e), false);
    }
    
    preventDefaults(e) {
        e.preventDefault();
        e.stopPropagation();
    }
    
    highlight() {
        document.body.classList.add('drag-highlight');
    }
    
    unhighlight() {
        document.body.classList.remove('drag-highlight');
    }
    
    handleDrop(e) {
        const files = e.dataTransfer.files;
        if (files.length > 0 && files[0].type === 'application/json') {
            // Simulate file input change
            const dataTransfer = new DataTransfer();
            dataTransfer.items.add(files[0]);
            this.elements.fileInput.files = dataTransfer.files;
            
            // Trigger change event
            const event = new Event('change', { bubbles: true });
            this.elements.fileInput.dispatchEvent(event);
        }
    }
}

// Add drag highlight styles
const style = document.createElement('style');
style.textContent = `
    body.drag-highlight {
        position: relative;
    }
    
    body.drag-highlight::after {
        content: 'Drop simulation file here';
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: rgba(76, 175, 80, 0.1);
        border: 3px dashed #4CAF50;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 2rem;
        color: #4CAF50;
        z-index: 9999;
        pointer-events: none;
    }
`;
document.head.appendChild(style);