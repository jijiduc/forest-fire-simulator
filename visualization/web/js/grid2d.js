// 2D Grid Renderer for Forest Fire Simulation

export class Grid2DRenderer {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        
        // Grid properties
        this.gridWidth = 0;
        this.gridHeight = 0;
        this.cellSize = 10;
        
        // View properties
        this.offsetX = 0;
        this.offsetY = 0;
        this.scale = 1.0;
        
        // Options
        this.showGrid = true;
        this.showElevation = false;
        
        // Colors
        this.colors = {
            empty: '#e0e0e0',
            tree: '#4CAF50',
            burning: ['#ffeb3b', '#ff9800', '#ff5722', '#d32f2f'], // Intensity gradient
            burnt: '#424242',
            grid: 'rgba(255, 255, 255, 0.1)'
        };
        
        // Mouse tracking
        this.isDragging = false;
        this.lastMouseX = 0;
        this.lastMouseY = 0;
        
        this.setupInteraction();
        this.resize();
    }
    
    setGridSize(width, height) {
        this.gridWidth = width;
        this.gridHeight = height;
        this.centerView();
    }
    
    setupInteraction() {
        // Mouse events
        this.canvas.addEventListener('mousedown', (e) => this.handleMouseDown(e));
        this.canvas.addEventListener('mousemove', (e) => this.handleMouseMove(e));
        this.canvas.addEventListener('mouseup', () => this.handleMouseUp());
        this.canvas.addEventListener('mouseleave', () => this.handleMouseUp());
        
        // Wheel event for zoom
        this.canvas.addEventListener('wheel', (e) => this.handleWheel(e));
        
        // Touch events for mobile
        this.canvas.addEventListener('touchstart', (e) => this.handleTouchStart(e));
        this.canvas.addEventListener('touchmove', (e) => this.handleTouchMove(e));
        this.canvas.addEventListener('touchend', () => this.handleTouchEnd());
        
        // Zoom controls
        const zoomInBtn = document.getElementById('zoomInBtn');
        const zoomOutBtn = document.getElementById('zoomOutBtn');
        const zoomResetBtn = document.getElementById('zoomResetBtn');
        
        if (zoomInBtn) zoomInBtn.addEventListener('click', () => this.zoom(1.2));
        if (zoomOutBtn) zoomOutBtn.addEventListener('click', () => this.zoom(0.8));
        if (zoomResetBtn) zoomResetBtn.addEventListener('click', () => this.resetView());
    }
    
    handleMouseDown(e) {
        this.isDragging = true;
        this.lastMouseX = e.clientX;
        this.lastMouseY = e.clientY;
        this.canvas.style.cursor = 'grabbing';
    }
    
    handleMouseMove(e) {
        if (!this.isDragging) return;
        
        const dx = e.clientX - this.lastMouseX;
        const dy = e.clientY - this.lastMouseY;
        
        this.offsetX += dx;
        this.offsetY += dy;
        
        this.lastMouseX = e.clientX;
        this.lastMouseY = e.clientY;
        
        this.render();
    }
    
    handleMouseUp() {
        this.isDragging = false;
        this.canvas.style.cursor = 'grab';
    }
    
    handleWheel(e) {
        e.preventDefault();
        
        const rect = this.canvas.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        const mouseY = e.clientY - rect.top;
        
        // Calculate zoom
        const zoomFactor = e.deltaY < 0 ? 1.1 : 0.9;
        const newScale = Math.max(0.1, Math.min(10, this.scale * zoomFactor));
        
        // Adjust offset to zoom towards mouse position
        const scaleRatio = newScale / this.scale;
        this.offsetX = mouseX - (mouseX - this.offsetX) * scaleRatio;
        this.offsetY = mouseY - (mouseY - this.offsetY) * scaleRatio;
        
        this.scale = newScale;
        this.render();
    }
    
    handleTouchStart(e) {
        if (e.touches.length === 1) {
            this.isDragging = true;
            this.lastMouseX = e.touches[0].clientX;
            this.lastMouseY = e.touches[0].clientY;
        }
    }
    
    handleTouchMove(e) {
        if (!this.isDragging || e.touches.length !== 1) return;
        e.preventDefault();
        
        const dx = e.touches[0].clientX - this.lastMouseX;
        const dy = e.touches[0].clientY - this.lastMouseY;
        
        this.offsetX += dx;
        this.offsetY += dy;
        
        this.lastMouseX = e.touches[0].clientX;
        this.lastMouseY = e.touches[0].clientY;
        
        this.render();
    }
    
    handleTouchEnd() {
        this.isDragging = false;
    }
    
    zoom(factor) {
        const centerX = this.canvas.width / 2;
        const centerY = this.canvas.height / 2;
        
        const newScale = Math.max(0.1, Math.min(10, this.scale * factor));
        const scaleRatio = newScale / this.scale;
        
        this.offsetX = centerX - (centerX - this.offsetX) * scaleRatio;
        this.offsetY = centerY - (centerY - this.offsetY) * scaleRatio;
        
        this.scale = newScale;
        this.render();
    }
    
    centerView() {
        if (this.gridWidth === 0 || this.gridHeight === 0) return;
        
        // Calculate scale to fit grid in canvas
        const scaleX = (this.canvas.width * 0.9) / (this.gridWidth * this.cellSize);
        const scaleY = (this.canvas.height * 0.9) / (this.gridHeight * this.cellSize);
        this.scale = Math.min(scaleX, scaleY);
        
        // Center the grid
        const gridPixelWidth = this.gridWidth * this.cellSize * this.scale;
        const gridPixelHeight = this.gridHeight * this.cellSize * this.scale;
        
        this.offsetX = (this.canvas.width - gridPixelWidth) / 2;
        this.offsetY = (this.canvas.height - gridPixelHeight) / 2;
    }
    
    resetView() {
        this.centerView();
        this.render();
    }
    
    resize() {
        const container = this.canvas.parentElement;
        this.canvas.width = container.clientWidth;
        this.canvas.height = container.clientHeight;
        
        this.centerView();
    }
    
    render(cells = []) {
        // Clear canvas
        this.ctx.fillStyle = '#1a1a1a';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        if (cells.length === 0) return;
        
        // Save context state
        this.ctx.save();
        
        // Apply transformations
        this.ctx.translate(this.offsetX, this.offsetY);
        this.ctx.scale(this.scale, this.scale);
        
        // Calculate visible area
        const visibleLeft = Math.max(0, Math.floor(-this.offsetX / (this.cellSize * this.scale)));
        const visibleTop = Math.max(0, Math.floor(-this.offsetY / (this.cellSize * this.scale)));
        const visibleRight = Math.min(this.gridWidth, Math.ceil((this.canvas.width - this.offsetX) / (this.cellSize * this.scale)));
        const visibleBottom = Math.min(this.gridHeight, Math.ceil((this.canvas.height - this.offsetY) / (this.cellSize * this.scale)));
        
        // Draw cells
        cells.forEach(cell => {
            // Skip cells outside visible area
            if (cell.x < visibleLeft || cell.x >= visibleRight ||
                cell.y < visibleTop || cell.y >= visibleBottom) {
                return;
            }
            
            const x = cell.x * this.cellSize;
            const y = cell.y * this.cellSize;
            
            // Get cell color
            let color = this.getCellColor(cell);
            
            // Apply elevation shading if enabled
            if (this.showElevation && cell.elevation) {
                color = this.applyElevationShading(color, cell.elevation);
            }
            
            // Draw cell
            this.ctx.fillStyle = color;
            this.ctx.fillRect(x, y, this.cellSize, this.cellSize);
        });
        
        // Draw grid lines if enabled
        if (this.showGrid && this.scale > 0.3) {
            this.drawGrid(visibleLeft, visibleTop, visibleRight, visibleBottom);
        }
        
        // Restore context state
        this.ctx.restore();
        
        // Draw scale indicator
        this.drawScaleIndicator();
    }
    
    getCellColor(cell) {
        switch (cell.state) {
            case 'Empty':
                return this.colors.empty;
            
            case 'Tree':
                // Vary green based on moisture
                const greenIntensity = 80 + Math.floor(cell.moisture * 40);
                return `rgb(76, ${greenIntensity}, 80)`;
            
            case 'Burning':
                // Color based on fire intensity
                const intensityIndex = Math.min(3, Math.floor(cell.fireIntensity * 4));
                return this.colors.burning[intensityIndex];
            
            case 'Burnt':
                return this.colors.burnt;
            
            default:
                // Handle complex burning state
                if (cell.state.includes('Burning')) {
                    const intensityIndex = Math.min(3, Math.floor(cell.fireIntensity * 4));
                    return this.colors.burning[intensityIndex];
                }
                return this.colors.empty;
        }
    }
    
    applyElevationShading(color, elevation) {
        // Simple elevation-based shading
        const factor = 0.7 + (elevation / 3000) * 0.3; // Assume max elevation ~3000m
        
        // Parse color and apply factor
        const rgb = this.hexToRgb(color) || this.parseRgb(color);
        if (rgb) {
            const r = Math.floor(rgb.r * factor);
            const g = Math.floor(rgb.g * factor);
            const b = Math.floor(rgb.b * factor);
            return `rgb(${r}, ${g}, ${b})`;
        }
        
        return color;
    }
    
    hexToRgb(hex) {
        const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : null;
    }
    
    parseRgb(rgb) {
        const result = /^rgb\((\d+),\s*(\d+),\s*(\d+)\)$/.exec(rgb);
        return result ? {
            r: parseInt(result[1]),
            g: parseInt(result[2]),
            b: parseInt(result[3])
        } : null;
    }
    
    drawGrid(left, top, right, bottom) {
        this.ctx.strokeStyle = this.colors.grid;
        this.ctx.lineWidth = 1 / this.scale;
        
        // Vertical lines
        for (let x = left; x <= right; x++) {
            this.ctx.beginPath();
            this.ctx.moveTo(x * this.cellSize, top * this.cellSize);
            this.ctx.lineTo(x * this.cellSize, bottom * this.cellSize);
            this.ctx.stroke();
        }
        
        // Horizontal lines
        for (let y = top; y <= bottom; y++) {
            this.ctx.beginPath();
            this.ctx.moveTo(left * this.cellSize, y * this.cellSize);
            this.ctx.lineTo(right * this.cellSize, y * this.cellSize);
            this.ctx.stroke();
        }
    }
    
    drawScaleIndicator() {
        // Draw scale bar in bottom left corner
        const barLength = 100;
        const realLength = barLength / this.scale / this.cellSize;
        
        this.ctx.save();
        this.ctx.strokeStyle = '#fff';
        this.ctx.fillStyle = '#fff';
        this.ctx.lineWidth = 2;
        
        const x = 20;
        const y = this.canvas.height - 30;
        
        // Draw bar
        this.ctx.beginPath();
        this.ctx.moveTo(x, y);
        this.ctx.lineTo(x + barLength, y);
        this.ctx.stroke();
        
        // Draw end caps
        this.ctx.beginPath();
        this.ctx.moveTo(x, y - 5);
        this.ctx.lineTo(x, y + 5);
        this.ctx.moveTo(x + barLength, y - 5);
        this.ctx.lineTo(x + barLength, y + 5);
        this.ctx.stroke();
        
        // Draw label
        this.ctx.font = '12px sans-serif';
        this.ctx.textAlign = 'center';
        this.ctx.fillText(`${realLength.toFixed(0)} cells`, x + barLength / 2, y - 10);
        
        this.ctx.restore();
    }
    
    exportImage(filename = 'forest_fire_grid.png') {
        // Create a temporary link element
        const link = document.createElement('a');
        link.download = filename;
        link.href = this.canvas.toDataURL('image/png');
        link.click();
    }
}