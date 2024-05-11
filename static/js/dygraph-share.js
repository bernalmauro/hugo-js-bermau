function convertDygraphToImage(dygraph) {
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
  
    canvas.width = dygraph.width;
    canvas.height = dygraph.height;
  
    dygraph.toCanvas(canvas);
  
    const imageURL = canvas.toDataURL('image/png'); // Obtiene la URL de la imagen
    return imageURL;
  }
  
  function shareDygraph(dygraph) {
    const imageURL = convertDygraphToImage(dygraph); // URL de la imagen
  
    // Opción 1: Abrir la imagen en una nueva pestaña
    window.open(imageURL, '_blank'); // Abre la imagen en una nueva pestaña
  
    // Opción 2: Generar un enlace directo a la imagen
    // const shareLink = document.createElement('a');
    // shareLink.href = imageURL;
    // shareLink.target = '_blank'; // Abre en una nueva pestaña
    // shareLink.textContent = 'Compartir Dygraph en redes sociales'; // Texto del enlace
  
    // // Puedes agregar el enlace a un elemento HTML en tu página para que sea visible al usuario.
    // document.getElementById('share-link').appendChild(shareLink);
  
    // Opción 3: Utilizar una biblioteca de JavaScript para compartir imágenes (ejemplo con ShareButtons.io)
    // **Instalación y configuración de ShareButtons.io (ver instrucciones anteriores)**
    // sbInit(); // Inicializar ShareButtons.io
    // const shareButtons = document.querySelectorAll('.sharing-buttons a');
    // shareButtons.forEach(button => {
    //   button.dataset.url = imageURL; // Establecer la URL de la imagen en cada botón
    // });
  }
  