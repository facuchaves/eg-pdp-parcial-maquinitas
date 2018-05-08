module TestParcialMaquinitas where 
import ParcialMaquinitas
import Test.Hspec

personaConPocaSuerte = CPersona { nombre = "Persona con poca suerte" , dinero = 100.0 , suerte = 30 , factores = [] , amuleto = 0 , pacienca = False , apuesta = 100.0 }
personaConMuchaSuerte = CPersona { nombre = "Persona con mucha suerte" , dinero = 100.0 , suerte = 99 , factores = [] , amuleto = 10 , pacienca = True , apuesta = 100.0 }
personaConMuchaSuerteSinPaciencia = CPersona { nombre = "Persona con mucha suerte" , dinero = 100.0 , suerte = 99 , factores = [] , amuleto = 10 , pacienca = False , apuesta = 100.0 }

jackpot50 = (jackpot 50)
casino = [ruleta , jackpot50]
grupoConUnGanadorYUnPerdedor = [personaConMuchaSuerte,personaConPocaSuerte]
grupoConDosGanadores = [personaConMuchaSuerte,personaConMuchaSuerteSinPaciencia]

runTests = hspec $ do
  describe "Tests punto 2" $ do
    it "Faltan Tests" $ do
      True `shouldBe` True

  describe "Tests punto 3" $ do
    
    it "Persona con poca suerte no gana ruleta" $ do
      puedeGanarJuego personaConPocaSuerte ruleta `shouldBe` False

    it "Persona con poca suerte no gana jackpot" $ do
      puedeGanarJuego personaConPocaSuerte jackpot50 `shouldBe` False
    
    it "Persona con mucha suerte gana ruleta" $ do
      puedeGanarJuego personaConMuchaSuerte ruleta `shouldBe` True

    it "Persona con mucha suerte gana jackpot" $ do
      puedeGanarJuego personaConMuchaSuerte jackpot50 `shouldBe` True

    it "Persona con mucha suerte pero sin paciencia no gana jackpot" $ do
      puedeGanarJuego personaConMuchaSuerteSinPaciencia jackpot50 `shouldBe` False

    it "Persona con mucha suerte pero sin paciencia gana ruleta" $ do
    puedeGanarJuego personaConMuchaSuerteSinPaciencia ruleta `shouldBe` True

  describe "Tests punto 4" $ do
    
    it "Persona con mucha suerte salta la banca en el casino" $ do
      apostarEnElCasino personaConMuchaSuerte casino `shouldBe` personaConMuchaSuerte { dinero = ( dinero personaConMuchaSuerte ) + ( ( dinero personaConMuchaSuerte ) * 37 ) + 50.0 }

    it "Persona con poca suerte no juega/gana nada en el casino" $ do
      apostarEnElCasino personaConPocaSuerte casino `shouldBe` personaConPocaSuerte

  describe "Tests punto 5" $ do
    it "Persona con mucha suerte pero sin paciencia gana en un casino con ruleta y jackpot" $ do
      puedeGanarAlMenosUnJuego casino personaConMuchaSuerteSinPaciencia `shouldBe` True
    it "Persona con poca suerte pierde en un casino con ruleta y jackpot" $ do
      puedeGanarAlMenosUnJuego casino personaConPocaSuerte `shouldBe` False
    it "Grupo de personas con mucha y poca suerte el perdedor es el que tiene poca suerte" $ do
      perdedoresDelGrupo casino grupoConUnGanadorYUnPerdedor `shouldBe` [personaConPocaSuerte]
    it "Grupo de personas con mucha suerte no hay perdedor." $ do
      perdedoresDelGrupo casino grupoConDosGanadores `shouldBe` []
      
  describe "Tests punto 6" $ do
    it "Persona con poca suerte pierde plata" $ do
      jugar personaConPocaSuerte ruleta `shouldBe` personaConPocaSuerte { dinero = 0.0 }
    it "Persona con mucha suerte gana plata" $ do
      jugar personaConMuchaSuerte ruleta `shouldBe` personaConMuchaSuerte { dinero = 3700.0 } -- TODO Ver si esto esta bien