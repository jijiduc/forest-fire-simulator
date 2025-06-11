#!/bin/bash
# Complete video generation pipeline for forest fire simulation demo

set -e  # Exit on error

echo "=== Forest Fire Simulation - Demo Video Generation ==="
echo

# Check dependencies
command -v python3 >/dev/null 2>&1 || { echo "Error: python3 is required but not installed." >&2; exit 1; }
command -v ffmpeg >/dev/null 2>&1 || { echo "Error: ffmpeg is required but not installed." >&2; exit 1; }

# Set paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"
EXPORT_DIR="${1:-$PROJECT_ROOT/output/video_export}"
VIDEO_DIR="${2:-$PROJECT_ROOT/output/video_frames}"
OUTPUT_VIDEO="${3:-$PROJECT_ROOT/output/demo_video.mp4}"

# Create output directories
mkdir -p "$VIDEO_DIR/2d_frames"
mkdir -p "$VIDEO_DIR/3d_frames"

echo "Export directory: $EXPORT_DIR"
echo "Video frames directory: $VIDEO_DIR"
echo "Output video: $OUTPUT_VIDEO"
echo

# Check if export data exists
if [ ! -d "$EXPORT_DIR/frames" ]; then
    echo "Error: No frame data found in $EXPORT_DIR"
    echo "Please run the VideoExportDemo first:"
    echo "  sbt 'runMain VideoExportDemo'"
    exit 1
fi

# Install Python dependencies if needed
echo "Checking Python dependencies..."
pip_install() {
    if ! python3 -c "import $1" 2>/dev/null; then
        echo "Installing $1..."
        pip3 install --user $2
    fi
}

pip_install numpy numpy
pip_install pandas pandas
pip_install matplotlib matplotlib
pip_install PIL pillow
pip_install plotly plotly
pip_install scipy scipy

echo

# Generate 2D frames
echo "Step 1: Generating 2D visualization frames..."
python3 "$SCRIPT_DIR/generate_frames.py" "$EXPORT_DIR" "$VIDEO_DIR/2d_frames" --processes 4

echo
echo "Step 2: Generating 3D terrain frames..."
# Install kaleido for plotly image export if needed
if ! python3 -c "import kaleido" 2>/dev/null; then
    echo "Installing kaleido for 3D rendering..."
    pip3 install --user kaleido
fi

python3 "$SCRIPT_DIR/terrain_3d.py" "$EXPORT_DIR" "$VIDEO_DIR/3d_frames"

echo
echo "Step 3: Compiling final video..."
python3 "$SCRIPT_DIR/compile_video.py" "$VIDEO_DIR" "$OUTPUT_VIDEO"

echo
echo "=== Video Generation Complete! ==="
echo "Output video: $OUTPUT_VIDEO"
echo

# Show video info
if [ -f "$OUTPUT_VIDEO" ]; then
    echo "Video properties:"
    ffprobe -v error -select_streams v:0 -show_entries stream=width,height,r_frame_rate,duration -of default=noprint_wrappers=1 "$OUTPUT_VIDEO"
    
    # Get file size
    size=$(ls -lh "$OUTPUT_VIDEO" | awk '{print $5}')
    echo "File size: $size"
fi

echo
echo "✓ Demo video ready for presentation!"