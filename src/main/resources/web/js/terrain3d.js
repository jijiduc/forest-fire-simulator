// 3D Terrain Renderer for Forest Fire Simulation

export class Terrain3DRenderer {
    constructor(container) {
        this.container = container;
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.terrain = null;
        this.gridWidth = 0;
        this.gridHeight = 0;
        
        // Materials cache
        this.materials = {
            empty: new THREE.MeshPhongMaterial({ color: 0xe0e0e0 }),
            tree: new THREE.MeshPhongMaterial({ color: 0x4CAF50 }),
            burning: [
                new THREE.MeshPhongMaterial({ color: 0xffeb3b, emissive: 0xffeb3b, emissiveIntensity: 0.5 }),
                new THREE.MeshPhongMaterial({ color: 0xff9800, emissive: 0xff9800, emissiveIntensity: 0.6 }),
                new THREE.MeshPhongMaterial({ color: 0xff5722, emissive: 0xff5722, emissiveIntensity: 0.7 }),
                new THREE.MeshPhongMaterial({ color: 0xd32f2f, emissive: 0xd32f2f, emissiveIntensity: 0.8 })
            ],
            burnt: new THREE.MeshPhongMaterial({ color: 0x424242 })
        };
        
        this.init();
    }
    
    init() {
        // Scene setup
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x1a1a1a);
        this.scene.fog = new THREE.Fog(0x1a1a1a, 100, 500);
        
        // Camera setup
        const aspect = this.container.clientWidth / this.container.clientHeight;
        this.camera = new THREE.PerspectiveCamera(60, aspect, 0.1, 1000);
        this.camera.position.set(50, 50, 50);
        
        // Renderer setup
        this.renderer = new THREE.WebGLRenderer({ antialias: true });
        this.renderer.setSize(this.container.clientWidth, this.container.clientHeight);
        this.renderer.shadowMap.enabled = true;
        this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
        this.container.appendChild(this.renderer.domElement);
        
        // Controls setup
        if (typeof THREE.OrbitControls !== 'undefined') {
            this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
            this.controls.enableDamping = true;
            this.controls.dampingFactor = 0.05;
            this.controls.screenSpacePanning = false;
            this.controls.minDistance = 10;
            this.controls.maxDistance = 500;
            this.controls.maxPolarAngle = Math.PI / 2;
        }
        
        // Lighting setup
        this.setupLighting();
        
        // Add coordinate helpers
        this.addHelpers();
    }
    
    setupLighting() {
        // Ambient light
        const ambientLight = new THREE.AmbientLight(0x404040, 0.5);
        this.scene.add(ambientLight);
        
        // Directional light (sun)
        const directionalLight = new THREE.DirectionalLight(0xffffff, 1);
        directionalLight.position.set(50, 100, 50);
        directionalLight.castShadow = true;
        
        directionalLight.shadow.camera.left = -100;
        directionalLight.shadow.camera.right = 100;
        directionalLight.shadow.camera.top = 100;
        directionalLight.shadow.camera.bottom = -100;
        directionalLight.shadow.camera.near = 0.5;
        directionalLight.shadow.camera.far = 500;
        
        directionalLight.shadow.mapSize.width = 2048;
        directionalLight.shadow.mapSize.height = 2048;
        
        this.scene.add(directionalLight);
        
        // Hemisphere light for better ambient
        const hemisphereLight = new THREE.HemisphereLight(0x87CEEB, 0x8B7355, 0.3);
        this.scene.add(hemisphereLight);
    }
    
    addHelpers() {
        // Grid helper
        const gridHelper = new THREE.GridHelper(100, 10, 0x444444, 0x222222);
        this.scene.add(gridHelper);
        
        // Axes helper (optional, for debugging)
        // const axesHelper = new THREE.AxesHelper(20);
        // this.scene.add(axesHelper);
    }
    
    initialize(width, height, terrainInfo) {
        this.gridWidth = width;
        this.gridHeight = height;
        
        // Remove existing terrain if any
        if (this.terrain) {
            this.scene.remove(this.terrain);
            this.terrain.geometry.dispose();
        }
        
        // Create terrain geometry
        const geometry = new THREE.PlaneGeometry(
            width,
            height,
            width - 1,
            height - 1
        );
        
        // If we have elevation data, apply it
        if (terrainInfo && terrainInfo.hasSwissData) {
            // We'll update vertices based on actual elevation data when rendering
            this.hasElevation = true;
            this.elevationScale = 50 / (terrainInfo.maxElevation - terrainInfo.minElevation);
            this.minElevation = terrainInfo.minElevation;
        } else {
            // Flat terrain
            this.hasElevation = false;
        }
        
        // Create material with vertex colors
        const material = new THREE.MeshPhongMaterial({
            vertexColors: true,
            side: THREE.DoubleSide,
            flatShading: true
        });
        
        // Create mesh
        this.terrain = new THREE.Mesh(geometry, material);
        this.terrain.rotation.x = -Math.PI / 2; // Rotate to horizontal
        this.terrain.position.set(0, 0, 0);
        this.terrain.castShadow = true;
        this.terrain.receiveShadow = true;
        
        this.scene.add(this.terrain);
        
        // Update camera position
        this.camera.position.set(width * 0.7, width * 0.5, height * 0.7);
        this.camera.lookAt(0, 0, 0);
        
        if (this.controls) {
            this.controls.target.set(0, 0, 0);
            this.controls.update();
        }
    }
    
    render(cells) {
        if (!this.terrain || cells.length === 0) return;
        
        const geometry = this.terrain.geometry;
        const positions = geometry.attributes.position;
        const colors = new Float32Array(positions.count * 3);
        
        // Create a map for quick cell lookup
        const cellMap = new Map();
        cells.forEach(cell => {
            cellMap.set(`${cell.x},${cell.y}`, cell);
        });
        
        // Update vertices and colors
        for (let y = 0; y < this.gridHeight; y++) {
            for (let x = 0; x < this.gridWidth; x++) {
                const index = y * this.gridWidth + x;
                const cell = cellMap.get(`${x},${y}`);
                
                if (cell) {
                    // Update elevation if available
                    if (this.hasElevation && cell.elevation) {
                        const normalizedElevation = (cell.elevation - this.minElevation) * this.elevationScale;
                        positions.setZ(index, normalizedElevation);
                    }
                    
                    // Update color based on cell state
                    const color = this.getCellColor(cell);
                    colors[index * 3] = color.r;
                    colors[index * 3 + 1] = color.g;
                    colors[index * 3 + 2] = color.b;
                } else {
                    // Default color for missing cells
                    colors[index * 3] = 0.88;
                    colors[index * 3 + 1] = 0.88;
                    colors[index * 3 + 2] = 0.88;
                }
            }
        }
        
        // Update geometry
        if (this.hasElevation) {
            positions.needsUpdate = true;
            geometry.computeVertexNormals();
        }
        
        // Set colors
        geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
        
        // Render the scene
        this.renderer.render(this.scene, this.camera);
    }
    
    getCellColor(cell) {
        switch (cell.state) {
            case 'Empty':
                return { r: 0.88, g: 0.88, b: 0.88 };
            
            case 'Tree':
                // Vary green based on moisture
                const greenIntensity = 0.31 + cell.moisture * 0.16;
                return { r: 0.3, g: greenIntensity, b: 0.31 };
            
            case 'Burning':
                // Color based on fire intensity
                const intensity = cell.fireIntensity || 0.5;
                if (intensity < 0.25) {
                    return { r: 1.0, g: 0.92, b: 0.23 }; // Yellow
                } else if (intensity < 0.5) {
                    return { r: 1.0, g: 0.6, b: 0.0 }; // Orange
                } else if (intensity < 0.75) {
                    return { r: 1.0, g: 0.34, b: 0.13 }; // Red-orange
                } else {
                    return { r: 0.83, g: 0.18, b: 0.18 }; // Red
                }
            
            case 'Burnt':
                return { r: 0.26, g: 0.26, b: 0.26 };
            
            default:
                // Handle complex burning state
                if (cell.state.includes('Burning')) {
                    const intensity = cell.fireIntensity || 0.5;
                    if (intensity < 0.5) {
                        return { r: 1.0, g: 0.6, b: 0.0 };
                    } else {
                        return { r: 1.0, g: 0.34, b: 0.13 };
                    }
                }
                return { r: 0.88, g: 0.88, b: 0.88 };
        }
    }
    
    animate() {
        if (this.controls) {
            this.controls.update();
        }
        this.renderer.render(this.scene, this.camera);
    }
    
    resize() {
        const width = this.container.clientWidth;
        const height = this.container.clientHeight;
        
        this.camera.aspect = width / height;
        this.camera.updateProjectionMatrix();
        
        this.renderer.setSize(width, height);
    }
    
    exportImage(filename = 'forest_fire_3d.png') {
        // Render one frame to ensure canvas is up to date
        this.renderer.render(this.scene, this.camera);
        
        // Export canvas as image
        const link = document.createElement('a');
        link.download = filename;
        link.href = this.renderer.domElement.toDataURL('image/png');
        link.click();
    }
}