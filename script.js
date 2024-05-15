function updateImage() {
    // Grab the values from the input fields
    const xCoordinate = document.getElementById("x-coordinate").value;
    const yCoordinate = document.getElementById("y-coordinate").value;
    const windowLength = document.getElementById("window-length").value;

    // Create the payload object
    const formdata = new FormData();
    formdata.append("x", xCoordinate);
    formdata.append("y", yCoordinate);
    formdata.append("w", windowLength);

    // Send a POST request to the /api/render endpoint
    fetch("/api/render", {
        method: "POST",
        body: formdata,
        redirect: "follow"
    })
        .then(response => response.blob())
        .then(blob => {
            // Create a URL for the blob object
            const imageUrl = URL.createObjectURL(blob);
            // Update the image source with the URL
            const image = document.querySelector(".image");
            image.src = imageUrl;
            image.alt = "Render of the mandelbrot set";
        })
        .catch(error => {
            console.error("Error:", error);
        });
}


function move(direction) {
    const windowLength = parseFloat(document.getElementById("window-length").value);
    const step = windowLength * 0.25;
    const xCoordinate = parseFloat(document.getElementById("x-coordinate").value);
    const yCoordinate = parseFloat(document.getElementById("y-coordinate").value);
    let newX, newY;

    switch (direction) {
        case "left":
            newX = xCoordinate - step;
            newY = yCoordinate;
            break;
        case "right":
            newX = xCoordinate + step;
            newY = yCoordinate;
            break;
        case "down":
            newX = xCoordinate;
            newY = yCoordinate + step;
            break;
        case "up":
            newX = xCoordinate;
            newY = yCoordinate - step;
            break;
        default:
            return;
    }

    document.getElementById("x-coordinate").value = newX;
    document.getElementById("y-coordinate").value = newY;
    updateImage();
}


function zoom(direction) {
    const windowLength = parseFloat(document.getElementById("window-length").value);
    let newWindowLength;
    if (direction === "in") {
        newWindowLength = windowLength * 0.75;
    } else if (direction === "out") {
        newWindowLength = windowLength * 1.25;
    } else {
        return;
    }
    document.getElementById("window-length").value = newWindowLength;
    updateImage();
}


function resetParameters() {
    document.getElementById("x-coordinate").value = -0.5;
    document.getElementById("y-coordinate").value = 0.0;
    document.getElementById("window-length").value = 3.0;
    updateImage();
}
