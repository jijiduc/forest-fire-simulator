// Forest Fire Simulation Viewer - Main Module

import { Grid2DRenderer } from './grid2d.js';
import { Terrain3DRenderer } from './terrain3d.js';
import { Controls } from './controls.js';

class ForestFireViewer {
    constructor() {
        this.simulationData = null;
        this.currentFrame = 0;
        this.isPlaying = false;
        this.playbackSpeed = 1.0;
        this.lastUpdateTime = 0;
        this.accumulatedTime = 0;
        
        // State tracking
        this.cellStates = new Map(); // Current state of all cells
        
        // Renderers
        this.grid2D = null;
        this.terrain3D = null;
        this.activeRenderer = '2D';
        
        // UI Elements
        this.elements = this.initializeElements();
        
        // Initialize
        this.init();
    }
    
    initializeElements() {
        return {
            // File input
            fileInput: document.getElementById('fileInput'),
            fileName: document.getElementById('fileName'),
            
            // Playback controls
            playPauseBtn: document.getElementById('playPauseBtn'),
            stepBackBtn: document.getElementById('stepBackBtn'),
            stepForwardBtn: document.getElementById('stepForwardBtn'),
            resetBtn: document.getElementById('resetBtn'),
            speedSlider: document.getElementById('speedSlider'),
            speedValue: document.getElementById('speedValue'),
            timelineSlider: document.getElementById('timelineSlider'),
            timeValue: document.getElementById('timeValue'),
            frameNumber: document.getElementById('frameNumber'),
            totalFrames: document.getElementById('totalFrames'),
            
            // Metrics
            activeFires: document.getElementById('activeFires'),
            burntArea: document.getElementById('burntArea'),
            largestCluster: document.getElementById('largestCluster'),
            treeDensity: document.getElementById('treeDensity'),
            percolation: document.getElementById('percolation'),
            
            // View controls
            toggle2D: document.getElementById('toggle2D'),
            toggle3D: document.getElementById('toggle3D'),
            showGrid: document.getElementById('showGrid'),
            showElevation: document.getElementById('showElevation'),
            exportImageBtn: document.getElementById('exportImageBtn'),
            
            // Containers
            canvas2D: document.getElementById('canvas2D'),
            canvas3D: document.getElementById('canvas3D'),
            gridCanvas: document.getElementById('gridCanvas'),
            threejsContainer: document.getElementById('threejsContainer'),
            loadingOverlay: document.getElementById('loadingOverlay'),
            
            // Info displays
            parametersDisplay: document.getElementById('parametersDisplay'),
            simulationInfo: document.getElementById('simulationInfo'),
            statusMessage: document.getElementById('statusMessage'),
            performanceInfo: document.getElementById('performanceInfo')
        };
    }
    
    init() {
        // Initialize renderers
        this.grid2D = new Grid2DRenderer(this.elements.gridCanvas);
        this.terrain3D = new Terrain3DRenderer(this.elements.threejsContainer);
        
        // Initialize controls
        this.controls = new Controls(this.elements, this);
        
        // Set up event listeners
        this.setupEventListeners();
        
        // Start animation loop
        this.animate();
        
        // Try to load from API, otherwise show file upload message
        this.loadFromAPI().catch(() => {
            this.setStatus('Ready - Load a simulation file to begin');
        });
    }
    
    setupEventListeners() {
        // File input
        this.elements.fileInput.addEventListener('change', (e) => this.handleFileSelect(e));
        
        // Playback controls
        this.elements.playPauseBtn.addEventListener('click', () => this.togglePlayback());
        this.elements.stepBackBtn.addEventListener('click', () => this.stepFrame(-1));
        this.elements.stepForwardBtn.addEventListener('click', () => this.stepFrame(1));
        this.elements.resetBtn.addEventListener('click', () => this.reset());
        
        // Speed control
        this.elements.speedSlider.addEventListener('input', (e) => {
            this.playbackSpeed = parseFloat(e.target.value);
            this.elements.speedValue.textContent = `${this.playbackSpeed}x`;
        });
        
        // Timeline control
        this.elements.timelineSlider.addEventListener('input', (e) => {
            if (this.simulationData) {
                this.goToFrame(parseInt(e.target.value));
            }
        });
        
        // View toggles
        this.elements.toggle2D.addEventListener('click', () => this.setActiveView('2D'));
        this.elements.toggle3D.addEventListener('click', () => this.setActiveView('3D'));
        
        // View options
        this.elements.showGrid.addEventListener('change', (e) => {
            this.grid2D.showGrid = e.target.checked;
            this.render();
        });
        
        this.elements.showElevation.addEventListener('change', (e) => {
            this.grid2D.showElevation = e.target.checked;
            this.render();
        });
        
        // Export image
        this.elements.exportImageBtn.addEventListener('click', () => this.exportImage());
        
        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => this.handleKeyPress(e));
        
        // Window resize
        window.addEventListener('resize', () => this.handleResize());
    }
    
    async handleFileSelect(event) {
        const file = event.target.files[0];
        if (!file) return;
        
        this.elements.fileName.textContent = file.name;
        this.showLoading(true);
        this.setStatus('Loading simulation data...');
        
        try {
            const text = await file.text();
            const data = JSON.parse(text);
            
            this.loadSimulation(data);
            this.setStatus('Simulation loaded successfully');
        } catch (error) {
            console.error('Error loading file:', error);
            this.setStatus(`Error loading file: ${error.message}`, 'error');
        } finally {
            this.showLoading(false);
        }
    }
    
    async loadFromAPI() {
        this.showLoading(true);
        this.setStatus('Loading simulation data from server...');
        
        try {
            const response = await fetch('/api/simulation/data');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            const data = await response.json();
            this.loadSimulation(data);
            this.setStatus('Simulation loaded from server');
            this.elements.fileName.textContent = 'Server simulation';
        } catch (error) {
            console.error('Failed to load from API:', error);
            throw error;
        } finally {
            this.showLoading(false);
        }
    }
    
    loadSimulation(data) {
        this.simulationData = data;
        this.currentFrame = 0;
        this.cellStates.clear();
        
        // Update UI with metadata
        this.updateMetadata();
        
        // Initialize grid state from first frame
        if (data.frames && data.frames.length > 0) {
            this.applyFrame(0);
        }
        
        // Enable controls
        this.enableControls(true);
        
        // Initialize renderers with grid size
        this.grid2D.setGridSize(data.metadata.width, data.metadata.height);
        
        if (data.metadata.terrain) {
            this.terrain3D.initialize(
                data.metadata.width,
                data.metadata.height,
                data.metadata.terrain
            );
        }
        
        // Render initial state
        this.render();
    }
    
    updateMetadata() {
        const { metadata } = this.simulationData;
        
        // Update timeline
        this.elements.timelineSlider.max = this.simulationData.frames.length - 1;
        this.elements.totalFrames.textContent = this.simulationData.frames.length;
        
        // Update parameters display
        const paramsHtml = Object.entries(metadata.parameters)
            .map(([key, value]) => `
                <div class="parameter-item">
                    <label>${this.formatParameterName(key)}:</label>
                    <span>${this.formatParameterValue(value)}</span>
                </div>
            `).join('');
        
        this.elements.parametersDisplay.innerHTML = paramsHtml;
        
        // Update simulation info
        this.elements.simulationInfo.innerHTML = `
            <div class="parameter-item">
                <label>Grid Size:</label>
                <span>${metadata.width} × ${metadata.height}</span>
            </div>
            <div class="parameter-item">
                <label>Time Steps:</label>
                <span>${metadata.timesteps}</span>
            </div>
            <div class="parameter-item">
                <label>Time Step:</label>
                <span>${metadata.deltaTime.toFixed(2)}s</span>
            </div>
            ${metadata.terrain ? `
            <div class="parameter-item">
                <label>Elevation Range:</label>
                <span>${metadata.terrain.minElevation.toFixed(0)}m - ${metadata.terrain.maxElevation.toFixed(0)}m</span>
            </div>
            ` : ''}
        `;
    }
    
    applyFrame(frameIndex) {
        if (!this.simulationData || frameIndex < 0 || frameIndex >= this.simulationData.frames.length) {
            return;
        }
        
        const frame = this.simulationData.frames[frameIndex];
        
        // Apply cell updates
        if (frame.fullFrame || frameIndex === 0) {
            // Full frame - replace all cells
            this.cellStates.clear();
            frame.cells.forEach(cell => {
                const key = `${cell.x},${cell.y}`;
                this.cellStates.set(key, cell);
            });
        } else {
            // Delta frame - update only changed cells
            frame.cells.forEach(cell => {
                const key = `${cell.x},${cell.y}`;
                this.cellStates.set(key, cell);
            });
        }
        
        // Update current frame
        this.currentFrame = frameIndex;
        
        // Update UI
        this.updateFrameUI(frame);
    }
    
    updateFrameUI(frame) {
        // Update timeline
        this.elements.timelineSlider.value = this.currentFrame;
        this.elements.timeValue.textContent = frame.time.toFixed(2);
        this.elements.frameNumber.textContent = this.currentFrame + 1;
        
        // Update metrics
        const { metrics } = frame;
        this.elements.activeFires.textContent = metrics.activeFires;
        this.elements.burntArea.textContent = metrics.burntArea;
        this.elements.largestCluster.textContent = metrics.largestCluster;
        this.elements.treeDensity.textContent = metrics.treeDensity.toFixed(3);
        this.elements.percolation.textContent = metrics.percolationIndicator.toFixed(3);
    }
    
    render() {
        if (!this.simulationData) return;
        
        // Convert cell states to array for renderers
        const cells = Array.from(this.cellStates.values());
        
        if (this.activeRenderer === '2D') {
            this.grid2D.render(cells);
        } else {
            this.terrain3D.render(cells);
        }
    }
    
    animate(timestamp = 0) {
        requestAnimationFrame((t) => this.animate(t));
        
        // Calculate delta time
        const deltaTime = timestamp - this.lastUpdateTime;
        this.lastUpdateTime = timestamp;
        
        // Update performance info
        if (deltaTime > 0) {
            const fps = 1000 / deltaTime;
            this.elements.performanceInfo.textContent = `${fps.toFixed(0)} FPS`;
        }
        
        // Update playback if playing
        if (this.isPlaying && this.simulationData) {
            this.accumulatedTime += deltaTime * this.playbackSpeed;
            
            const frameTime = this.simulationData.metadata.deltaTime * 1000; // Convert to ms
            
            if (this.accumulatedTime >= frameTime) {
                this.accumulatedTime -= frameTime;
                this.stepFrame(1);
            }
        }
        
        // Update 3D renderer if active
        if (this.activeRenderer === '3D') {
            this.terrain3D.animate();
        }
    }
    
    togglePlayback() {
        this.isPlaying = !this.isPlaying;
        this.elements.playPauseBtn.textContent = this.isPlaying ? '⏸ Pause' : '▶ Play';
        this.elements.playPauseBtn.classList.toggle('active', this.isPlaying);
    }
    
    stepFrame(direction) {
        const newFrame = this.currentFrame + direction;
        if (newFrame >= 0 && newFrame < this.simulationData.frames.length) {
            this.goToFrame(newFrame);
        }
    }
    
    goToFrame(frameIndex) {
        // If jumping far, we might need to reconstruct state
        if (Math.abs(frameIndex - this.currentFrame) > 10) {
            // Rebuild state from beginning
            this.cellStates.clear();
            for (let i = 0; i <= frameIndex; i++) {
                this.applyFrame(i);
            }
        } else {
            // Step through frames
            const direction = frameIndex > this.currentFrame ? 1 : -1;
            while (this.currentFrame !== frameIndex) {
                this.applyFrame(this.currentFrame + direction);
            }
        }
        
        this.render();
    }
    
    reset() {
        this.isPlaying = false;
        this.elements.playPauseBtn.textContent = '▶ Play';
        this.elements.playPauseBtn.classList.remove('active');
        this.goToFrame(0);
    }
    
    setActiveView(view) {
        this.activeRenderer = view;
        
        // Update UI
        this.elements.toggle2D.classList.toggle('active', view === '2D');
        this.elements.toggle3D.classList.toggle('active', view === '3D');
        this.elements.canvas2D.classList.toggle('active', view === '2D');
        this.elements.canvas3D.classList.toggle('active', view === '3D');
        
        // Render with new renderer
        this.render();
        this.handleResize();
    }
    
    exportImage() {
        if (this.activeRenderer === '2D') {
            this.grid2D.exportImage(`forest_fire_frame_${this.currentFrame}.png`);
        } else {
            this.terrain3D.exportImage(`forest_fire_3d_frame_${this.currentFrame}.png`);
        }
        this.setStatus('Image exported successfully');
    }
    
    handleKeyPress(event) {
        switch (event.key) {
            case ' ':
                event.preventDefault();
                this.togglePlayback();
                break;
            case 'ArrowLeft':
                this.stepFrame(-1);
                break;
            case 'ArrowRight':
                this.stepFrame(1);
                break;
            case 'f':
            case 'F':
                this.toggleFullscreen();
                break;
        }
    }
    
    toggleFullscreen() {
        if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen();
        } else {
            document.exitFullscreen();
        }
    }
    
    handleResize() {
        if (this.activeRenderer === '2D') {
            this.grid2D.resize();
        } else {
            this.terrain3D.resize();
        }
    }
    
    enableControls(enabled) {
        const controls = [
            this.elements.playPauseBtn,
            this.elements.stepBackBtn,
            this.elements.stepForwardBtn,
            this.elements.resetBtn,
            this.elements.timelineSlider
        ];
        
        controls.forEach(control => {
            control.disabled = !enabled;
        });
    }
    
    showLoading(show) {
        this.elements.loadingOverlay.classList.toggle('active', show);
    }
    
    setStatus(message, type = 'info') {
        this.elements.statusMessage.textContent = message;
        this.elements.statusMessage.className = type;
    }
    
    formatParameterName(name) {
        return name
            .replace(/([A-Z])/g, ' $1')
            .replace(/^./, str => str.toUpperCase())
            .trim();
    }
    
    formatParameterValue(value) {
        if (typeof value === 'number') {
            return value.toFixed(3);
        }
        return value;
    }
}

// Initialize viewer when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    window.viewer = new ForestFireViewer();
});