# Still Extraction
# Author: Robin Elahi, adapted by Julia DiCicco
# Input: folder of paired benthic and kelp video transects
# Output: nested folder structure containing 25 equidistant frames from each benthic transect
# Version 2
# 05/03/2026 Julia altered code to extract evenly spaced frames, rather than random frames
# 09/02/2026 added header and removed unused lines

import cv2
import os

# Get the current directory
current_directory = os.getcwd()

# List all MPG files in the current directory that start with 'b'
video_files = [f for f in os.listdir() if f.endswith("b.MP4")]

# Create an output directory in the current directory
output_directory = 'extracted_frames'
os.makedirs(output_directory, exist_ok=True)

# Function to extract frames from a video and save them
def extract_frames(video_path, output_folder, num_extract=25):
    # Create an output folder with the same name as the video (without extension)
    video_name = os.path.splitext(os.path.basename(video_path))[0]
    output_folder = os.path.join(output_directory, video_name)
    os.makedirs(output_folder, exist_ok=True)

    cap = cv2.VideoCapture(video_path)
    frame_count = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))

    # Select 25 equidistant frames to extract
    frame_interval = frame_count // (num_extract + 1)
    print(frame_interval)
    frames_to_extract = [frame_interval * i for i in range(1, num_extract + 1)]
    print(frames_to_extract)


    for frame_index in frames_to_extract:
        cap.set(cv2.CAP_PROP_POS_FRAMES, frame_index)
        ret, frame = cap.read()

        if ret:
            frame_filename = os.path.join(output_folder, f'{video_name}_frame_{frame_index}.jpg')
            cv2.imwrite(frame_filename, frame)

    cap.release()

# Extract frames from each video file in the current directory
for video_file in video_files:
    extract_frames(video_file, output_directory)