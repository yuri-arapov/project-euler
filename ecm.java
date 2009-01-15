// <XMP>
// Elliptic Curve Method (ECM) Prime Factorization
//
// Written by Dario Alejandro Alpern (Buenos Aires - Argentina)
// Last updated April 30th, 2005. See http://www.alpertron.com.ar/ECM.HTM
//
// Based in Yuji Kida's implementation for UBASIC interpreter
//
// No part of this code can be used for commercial purposes without
// the written consent from the author. Otherwise it can be used freely
// except that you have to write somewhere in the code this header.
// 
import java.applet.*;
import java.awt.*;
import java.math.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;

public class ecm extends Applet implements Runnable
{
  boolean onlyFactoring = true;
  static final int actionTextNbr = 1;
  static final int actionTextCurve = 2;
  static final int actionBtnCurve = 3;
  static final int actionTextFactor = 4;
  static final int actionBtnFactor = 5;

  static final int TYP_AURIF = 100000000;
  static final int TYP_TABLE = 150000000;
  static final int TYP_SIQS = 200000000;
  static final int TYP_LEHMAN = 250000000;
  static final int TYP_EC = 300000000;

  int digitsInGroup = 6;
  final BigInteger SS[] = new BigInteger[4000]; // For intermediate factors 
  final BigInteger PD[] = new BigInteger[4000]; // and prime factors
  final int Exp[] = new int[4000];
  final int Typ[] = new int[4000];
  final BigInteger PD1[] = new BigInteger[4000];
  final int Exp1[] = new int[4000];
  final int Typ1[] = new int[4000];
  String inputStr;
  boolean foundByLehman;
  boolean performLehman;
  static final BigInteger BigInt0 = BigInteger.valueOf(0L);
  static final BigInteger BigInt1 = BigInteger.valueOf(1L);
  static final BigInteger BigInt2 = BigInteger.valueOf(2L);
  static final BigInteger BigInt3 = BigInteger.valueOf(3L);
  static final int PWmax = 32, Qmax = 30241, LEVELmax = 11;
  static final int NLen = 1200;
  final int aiIndx[] = new int[Qmax];
  final int aiF[] = new int[Qmax];
  static final int aiP[] = { 2, 3, 5, 7, 11, 13 };
  static final int aiQ[] =
    {
      2,
      3,
      5,
      7,
      13,
      11,
      31,
      61,
      19,
      37,
      181,
      29,
      43,
      71,
      127,
      211,
      421,
      631,
      41,
      73,
      281,
      2521,
      17,
      113,
      241,
      337,
      1009,
      109,
      271,
      379,
      433,
      541,
      757,
      2161,
      7561,
      15121,
      23,
      67,
      89,
      199,
      331,
      397,
      463,
      617,
      661,
      881,
      991,
      1321,
      2311,
      2377,
      2971,
      3697,
      4159,
      4621,
      8317,
      9241,
      16633,
      18481,
      23761,
      101,
      151,
      401,
      601,
      701,
      1051,
      1201,
      1801,
      2801,
      3301,
      3851,
      4201,
      4951,
      6301,
      9901,
      11551,
      12601,
      14851,
      15401,
      19801,
      97,
      353,
      673,
      2017,
      3169,
      3361,
      5281,
      7393,
      21601,
      30241,
      53,
      79,
      131,
      157,
      313,
      521,
      547,
      859,
      911,
      937,
      1093,
      1171,
      1249,
      1301,
      1873,
      1951,
      2003,
      2081,
      41,
      2731,
      2861,
      3121,
      3433,
      3511,
      5851,
      6007,
      6553,
      7151,
      7723,
      8009,
      8191,
      8581,
      8737,
      9829,
      11701,
      13729,
      14561,
      15601,
      16381,
      17551,
      20021,
      20593,
      21841,
      24571,
      25741,
      26209,
      28081 };
  static final int aiG[] =
    {
      1,
      2,
      2,
      3,
      2,
      2,
      3,
      2,
      2,
      2,
      2,
      2,
      3,
      7,
      3,
      2,
      2,
      3,
      6,
      5,
      3,
      17,
      3,
      3,
      7,
      10,
      11,
      6,
      6,
      2,
      5,
      2,
      2,
      23,
      13,
      11,
      5,
      2,
      3,
      3,
      3,
      5,
      3,
      3,
      2,
      3,
      6,
      13,
      3,
      5,
      10,
      5,
      3,
      2,
      6,
      13,
      15,
      13,
      7,
      2,
      6,
      3,
      7,
      2,
      7,
      11,
      11,
      3,
      6,
      2,
      11,
      6,
      10,
      2,
      7,
      11,
      2,
      6,
      13,
      5,
      3,
      5,
      5,
      7,
      22,
      7,
      5,
      7,
      11,
      2,
      3,
      2,
      5,
      10,
      3,
      2,
      2,
      17,
      5,
      5,
      2,
      7,
      2,
      10,
      3,
      5,
      3,
      7,
      3,
      2,
      7,
      5,
      7,
      2,
      3,
      10,
      7,
      3,
      3,
      17,
      6,
      5,
      10,
      6,
      23,
      6,
      23,
      2,
      3,
      3,
      5,
      11,
      7,
      6,
      11,
      19 };
  final int aiInv[] = new int[PWmax];
  static final int aiNP[] = { 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6 };
  static final int aiNQ[] = { 5, 8, 11, 18, 22, 27, 36, 59, 79, 89, 136 };
  static final int aiT[] =
    {
      2 * 2 * 3,
      2 * 2 * 3 * 5,
      2 * 2 * 3 * 3 * 5,
      2 * 2 * 3 * 3 * 5 * 7,
      2 * 2 * 2 * 3 * 3 * 5 * 7,
      2 * 2 * 2 * 2 * 3 * 3 * 5 * 7,
      2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 7,
      2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 7 * 11,
      2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 7 * 11,
      2 * 2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 7 * 11,
      2 * 2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 7 * 11 * 13 };
  final long biTmp[] = new long[NLen];
  final long biExp[] = new long[NLen];
  final long biN[] = new long[NLen];
  final long biR[] = new long[NLen];
  final long biS[] = new long[NLen];
  final long biT[] = new long[NLen];
  final long biU[] = new long[NLen]; /* Temp */
  final long biV[] = new long[NLen]; /* Temp */
  final long biW[] = new long[NLen]; /* Temp */
  final long aiJS[][] = new long[PWmax][NLen];
  final long aiJW[][] = new long[PWmax][NLen];
  final long aiJX[][] = new long[PWmax][NLen];
  final long aiJ0[][] = new long[PWmax][NLen];
  final long aiJ1[][] = new long[PWmax][NLen];
  final long aiJ2[][] = new long[PWmax][NLen];
  final long aiJ00[][] = new long[PWmax][NLen];
  final long aiJ01[][] = new long[PWmax][NLen];
  int NumberLength; /* Length of multiple precision nbrs */
  int NbrFactors, NbrFactors1;
  int EC; /* Elliptic Curve number */
  /* Used inside GCD calculations in multiple precision numbers */
  final long CalcAuxGcdU[] = new long[NLen];
  final long CalcAuxGcdV[] = new long[NLen];
  final long CalcAuxGcdT[] = new long[NLen];
  final long CalcBigNbr[] = new long[NLen];
  long TestNbr[] = new long[NLen];
  final long TestNbr2[] = new long[NLen];
  final long GcdAccumulated[] = new long[NLen];
  final long Gamma[] = new long[386];
  final long Delta[] = new long[386];
  final long AurifQ[] = new long[386];
  final BigInteger Factores[] = new BigInteger[200];
  long[] fieldAA, fieldTX, fieldTZ, fieldUX, fieldUZ;
  long[] fieldAux1, fieldAux2, fieldAux3, fieldAux4;
  int NroFact, DegreeAurif;
  static final long DosALa32 = (long) 1 << 32;
  static final long DosALa31 = (long) 1 << 31;
  static final long DosALa31_1 = DosALa31 - 1;
  static final long DosALa63 = DosALa32 * DosALa31;
  static final double dDosALa31 = (double) DosALa31;
  static final double dDosALa62 = dDosALa31 * dDosALa31;
  static final long Mi = 1000000000;
  double dN;
  final long BigNbr1[] = new long[NLen];
  final int SmallPrime[] = new int[670]; /* Primes < 5000 */
  final long MontgomeryMultR1[] = new long[NLen];
  final long MontgomeryMultR2[] = new long[NLen];
  final long MontgomeryMultAfterInv[] = new long[NLen];
  long MontgomeryMultN;
  final double ProbArray[] = new double[6];
  TextArea upperTextArea;
  TextArea lowerTextArea;
  Label labelStatus;
  Label labelBottom;
  Label labelTop;
  TextField textNumber;
  TextField textCurve;
  TextField textFactor;
  Button btnCurve;
  Button btnFactor;
  BigInteger NumberToFactor;
  String StringToLabel;
  StringBuffer outputStr;
  boolean batchFinished = true;
  boolean batchPrime = false;
  volatile Thread calcThread;
  String textAreaContents;
  long OldTimeElapsed;
  long Old;
  long yieldFreq;
  boolean TerminateThread = true;
  int NextEC = -1;
  BigInteger InputFactor = BigInt0;
  int StepECM;
  int indexM, maxIndexM;
  int NbrPrimes, indexPrimes;
  BigInteger Quad1, Quad2, Quad3, Quad4;
  boolean Computing3Squares;
  long polynomialsSieved;
  long trialDivisions;
  long smoothsFound;
  long totalPartials;
  long partialsFound;
  long primeModMult;
  long lModularMult;
  long ValuesSieved;
  /* ECM limits for 30, 35, ..., 85 digits */
  static final int limits[] = { 5, 8, 15, 25, 27, 32, 43, 70, 150, 300, 350, 600, 1500 };
  static final String[] expressionText =
    {
      "Number too low (less than 2).",
      "Number too high (more than 10000 digits).",
      "Intermediate expression too high (more than 20000 digits).",
      "Non-integer division.",
      "Parentheses mismatch.",
      "Syntax error.",
      "Too many parentheses.",
      "Invalid parameter." };

  public static void main(String args[])
  {
    ecm ecm1 = new ecm();
    BigInteger big1, big2, big3;
    ecm1.init();
    int exp=344;
    big3 = new BigInteger("10").pow(exp).divide(BigInteger.valueOf(9));
    ecm1.BigNbrToBigInt(big3);
    System.out.println("Initial number = 10^"+exp+" / 9");
    System.out.println("NumberLength = "+ecm1.NumberLength);
    System.out.println("TestNbr[0] = "+ecm1.TestNbr[0]);
    System.out.println("TestNbr[1] = "+ecm1.TestNbr[1]);
    System.out.println(ecm1.BigIntToBigNbr(ecm1.TestNbr));
    ecm1.TerminateThread = false;
//    ecm1.GetMontgomeryParms();
//    big1 = ecm1.BigIntToBigNbr(ecm1.MontgomeryMultR2);
//    System.out.println("ANTES ModInvBigNbr: "+big1.toString());
//    ecm1.ModInvBigNbr(ecm1.MontgomeryMultR2, ecm1.MontgomeryMultR2, ecm1.TestNbr);
//    big2 = ecm1.BigIntToBigNbr(ecm1.MontgomeryMultR2);
//    System.out.println("DESPUES ModInvBigNbr: "+big2.toString());
//    System.out.println(big1.multiply(big2).mod(big3).toString());
    ecm1.MultBigNbrByLong(ecm1.TestNbr, -7, ecm1.TestNbr);
    ecm1.MultBigNbrByLong(ecm1.TestNbr, -7, ecm1.TestNbr);
    ecm1.DivBigNbrByLong(ecm1.TestNbr, -7, ecm1.TestNbr);
    ecm1.DivBigNbrByLong(ecm1.TestNbr, -7, ecm1.TestNbr);
    big1 = ecm1.BigIntToBigNbr(ecm1.TestNbr);
    System.out.println(big1.toString());
    ecm1.BigNbrToBigInt(big3);
    ecm1.LongToBigNbr(-7, ecm1.MontgomeryMultR2);
    ecm1.MultBigNbr(ecm1.MontgomeryMultR2, ecm1.TestNbr, ecm1.MontgomeryMultR1);
    ecm1.MultBigNbr(ecm1.MontgomeryMultR1, ecm1.MontgomeryMultR2, ecm1.TestNbr);
    big1 = ecm1.BigIntToBigNbr(ecm1.TestNbr);
    System.out.println(big1.toString());
  }

  public void init()
  {
    if (onlyFactoring)
    {
      layout(
        "Type number or numerical expression to factor here and press Return:",
        true);
    }
  }

  public int getFactors(
    BigInteger NbrToFactor,
    BigInteger[] Primes,
    int[] Exponents)
  {
    layout("Number to factor:", false);
    textNumber.setText(NbrToFactor.toString());
    startNewFactorization(true);       // Request complete factorization
    System.arraycopy(PD, 0, Primes, 0, NbrFactors);
    System.arraycopy(Exp, 0, Exponents, 0, NbrFactors);
    return NbrFactors;
  }

  void layout(String caption, boolean editable)
  {
    setBackground(Color.lightGray);
    setLayout(null);
    labelTop = new Label(caption);
    labelTop.reshape(10, 10, 570, 14);
    labelTop.setFont(new Font("Courier", Font.PLAIN, 12));
    labelTop.setAlignment(Label.CENTER);
    add(labelTop);
    textNumber = new TextField(1000);
    textNumber.reshape(10, 30, 570, 30);
    textNumber.setEditable(editable);
    add(textNumber);
    upperTextArea = new TextArea("", 7, 75, TextArea.SCROLLBARS_VERTICAL_ONLY);
    upperTextArea.reshape(10, 70, 570, 125);
    upperTextArea.setEditable(false);
    upperTextArea.setFont(new Font("Courier", Font.PLAIN, 12));
    add(upperTextArea);
    lowerTextArea = new TextArea("", 6, 75, TextArea.SCROLLBARS_VERTICAL_ONLY);
    lowerTextArea.reshape(10, 205, 570, 100);
    lowerTextArea.setEditable(false);
    lowerTextArea.setFont(new Font("Courier", Font.PLAIN, 12));
    add(lowerTextArea);
    labelStatus = new Label("");
    labelStatus.reshape(10, 315, 570, 14);
    labelStatus.setFont(new Font("Courier", Font.PLAIN, 12));
    add(labelStatus);
    textCurve = new TextField(10);
    textCurve.reshape(10, 335, 80, 30);
    add(textCurve);
    btnCurve = new Button("New curve");
    btnCurve.reshape(100, 335, 80, 30);
    btnCurve.setFont(new Font("Courier", Font.PLAIN, 12));
    add(btnCurve);
    textFactor = new TextField(200);
    textFactor.reshape(250, 335, 240, 30);
    add(textFactor);
    btnFactor = new Button("Factor");
    btnFactor.reshape(500, 335, 70, 30);
    btnFactor.setFont(new Font("Courier", Font.PLAIN, 12));
    add(btnFactor);
    labelBottom =
      new Label("Written by Dario Alejandro Alpern. Last updated April 30th, 2005");
    labelBottom.reshape(10, 370, 570, 14);
    labelBottom.setFont(new Font("Courier", Font.PLAIN, 12));
    labelBottom.setAlignment(Label.CENTER);
    add(labelBottom);
    if (onlyFactoring)
    {
      textNumber.addActionListener(new Command(actionTextNbr, this));
      textCurve.addActionListener(new Command(actionTextCurve, this));
      btnCurve.addActionListener(new Command(actionBtnCurve, this));
      textFactor.addActionListener(new Command(actionTextFactor, this));
      btnFactor.addActionListener(new Command(actionBtnFactor, this));
    }
    validate();
    textNumber.requestFocus();
    ProbArray[0] = 5E4;
    ProbArray[1] = 9.9E5;
    ProbArray[2] = 1.5E7;
    ProbArray[3] = 1.75E8;
    ProbArray[4] = 1.8E9;
    ProbArray[5] = 1.53E10;
  }

  public void destroy()
  { /* Applet end */
    TerminateThread = true;
  }

  public int setState(String state)
  {
    if (onlyFactoring)
    {
      int index = 0;
      int indexNew;
      int indexEnd = state.length();
      int indexComma = state.indexOf(',', index);
      int indexPlus, indexPlus2;
      int indexTimes;
      int indexExp;
      int indexParen;
      int i;

      if (indexComma == -1)
      {
        indexComma = indexEnd;
      }
      NumberToFactor = BigInt1;
      NbrFactors = 0;
      for (i = 0; i < 400; i++)
      {
        Exp[i] = Typ[i] = 0;
      }
      while (index < indexComma)
      {
        indexTimes = state.indexOf('x', index);
        if (indexTimes == -1)
        {
          indexTimes = indexComma;
        }
        indexExp = state.indexOf('^', index);
        indexParen = state.indexOf('(', index);
        if (indexParen > indexTimes || indexParen == -1)
        {
          indexParen = indexTimes;
        }
        if (indexExp > indexParen || indexExp == -1)
        {
          indexExp = indexParen;
        }
        indexNew = indexTimes + 1;
        switch (state.charAt(indexTimes - 1))
        {
          case ')' :
            Typ[NbrFactors] =
              Integer.parseInt(state.substring(indexParen + 1, indexTimes - 1));
            break;
          default :
            Typ[NbrFactors] = 0; /* Prime */
        }
        PD[NbrFactors] = new BigInteger(state.substring(index, indexExp));
        if (indexExp == indexParen)
        {
          Exp[NbrFactors] = 1;
        }
        else
        {
          Exp[NbrFactors] =
            Integer.parseInt(state.substring(indexExp + 1, indexParen));
        }
        NumberToFactor =
          NumberToFactor.multiply(PD[NbrFactors].pow(Exp[NbrFactors]));
        NbrFactors++;
        index = indexNew;
      } /* end while */
      lModularMult = -1;
      if (indexComma < indexEnd)
      {
        indexPlus = state.indexOf('+', indexComma);
        if (indexPlus > 0)
        {
          OldTimeElapsed =
            Long.parseLong(state.substring(indexComma + 1, indexPlus));
          indexPlus2 = state.indexOf('+', indexPlus + 1);
          if (indexPlus2 > 0)
          {
            lModularMult =
              Long.parseLong(state.substring(indexPlus + 1, indexPlus2));
            digitsInGroup = Integer.parseInt(state.substring(indexPlus2 + 1));
          }
          else
          {
            lModularMult = Long.parseLong(state.substring(indexPlus + 1));
          }
        }
        else
        {
          OldTimeElapsed = Long.parseLong(state.substring(indexComma + 1));
        }
      }
      else
      {
        OldTimeElapsed = -1; /* Old cookie format */
      }
      if (NumberToFactor.compareTo(BigInt1) > 0)
      {
        textNumber.setText(NumberToFactor.toString());
        calcThread = new Thread(this);
        calcThread.start();
      }
      return digitsInGroup; /* Number of digits in a group */
    }
    else
    {
      return 0;
    }
  }

  public void setDigits(int nbrDigits)
  {
    if (onlyFactoring)
    {
      digitsInGroup = (digitsInGroup & 0xFC00) | nbrDigits;
      String Tmp1 = textAreaContents;
      String Tmp2 = StringToLabel;
      ShowUpperPane();
      textAreaContents = Tmp1;
      StringToLabel = Tmp2;
    }
  }

  public void switchSIQS(int newSwitchSIQS)
  {
    if (newSwitchSIQS == 1)
    {
      digitsInGroup &= 0xFBFF;
    }
    else
    {
      digitsInGroup |= 0x0400;
    }
  }

  public void useCunnTable(int newUseCunnTable)
  {
    if (newUseCunnTable == 1)
    {
      digitsInGroup &= 0xEFFF;
    }
    else
    {
      digitsInGroup |= 0x1000;
    }
  }

  public void Verbose(int verboseType)
  {
    if (verboseType == 0)
    {
      digitsInGroup &= 0xF7FF;
    }
    else
    {
      digitsInGroup |= 0x0800;
    }
    String Tmp1 = textAreaContents;
    String Tmp2 = StringToLabel;
    ShowUpperPane();
    textAreaContents = Tmp1;
    StringToLabel = Tmp2;
  }

  public String getState()
  {
    if (onlyFactoring)
    {

      int i;
      String state = "";

      if (calcThread != null)
      {
        TerminateThread = true;
        try
        {
          calcThread.join(); /* Wait until the factorization thread dies */
        }
        catch (InterruptedException ie)
        {
        };
      }
      for (i = 0; i < (Computing3Squares ? NbrFactors1 : NbrFactors); i++)
      {
        if (i != 0)
        {
          state += "x";
        }
        state += PD[i].toString();
        if (Exp[i] != 1)
        {
          state += "^" + Exp[i];
        }
        if (Typ[i] != 0)
        {
          state += "(" + Typ[i] + ")";
        }
      }
      if (OldTimeElapsed >= 0)
      {
        state += "," + OldTimeElapsed;
      }
      if (lModularMult >= 0)
      {
        state += "+" + lModularMult;
      }
      state += "+" + digitsInGroup;
      return state;
    }
    else
    {
      return null;
    }
  }

  public String getStringsFromBothPanes()
  {
    if (onlyFactoring)
    {
      String lower = lowerTextArea.getText();
      String text =
        "<HTML><TITLE>Elliptic Curve Method factorization</TITLE><BODY><H2>Elliptic Curve Method factorization</H2><P><I>Written by Dario Alejandro Alpern (alpertron@hotmail.com)</I><P><PRE>";
      text += upperTextArea.getText() + "<HR><P>" + lower;
      if (lower.indexOf("complete") < 0)
      {
        long New = System.currentTimeMillis();
        if (OldTimeElapsed >= 0)
        {
          OldTimeElapsed += New - Old;
          Old = New;
          text += "<HR><P>Elapsed time: "
            + GetDHMS(OldTimeElapsed / 1000)
            + (lModularMult >= 0
              ? "\n" + lModularMult + " modular multiplications have been done"
              : "");
        }
      }
      text += "</PRE><BODY></HTML>";
      return text;
    }
    else
    {
      return null;
    }
  }

  String GetDHMS(long time)
  {
    return time / 86400 + "d " + (time % 86400) / 3600 + "h " +
           ((time % 3600) / 60) + "m " + (time % 60) + "s";
  }

  void addStringToLabel(String value)
  {
    if (value.length() + 1 + StringToLabel.length() >= 79)
    {
      textAreaContents += StringToLabel + "\n";
      StringToLabel = value + " ";
    }
    else
    {
      StringToLabel += value + " ";
    }
  }

  void ShowUpperPane()
  {
    int i;

    textAreaContents = "";
    StringToLabel = "";
    insertBigNbr(NumberToFactor);
    if (NbrFactors == 1 && Exp[0] == 1)
    {
      if (Typ[0] > 0)
      {
        addStringToLabel("is composite");
      }
      else
      {
        if (Typ[0] < 0)
        {
          addStringToLabel("is unknown");
        }
        else
        {
          addStringToLabel("is prime");
        }
      }
    }
    else
    {
      addStringToLabel("=");
      for (i = 0; i < NbrFactors; i++)
      {
        if (i != 0)
        {
          addStringToLabel("x");
        }
        insertBigNbr(PD[i]);
        if (Exp[i] != 1)
        {
          addStringToLabel("^");
          addStringToLabel("" + Exp[i]);
        }
        if (Typ[i] > 0)
        {
          if ((digitsInGroup & 0x800) == 0x800)
          {
            if (Typ[i] == TYP_AURIF)
            {
              addStringToLabel("(Aurifeuille)");
            }
            else if (Typ[i] / 50000000 * 50000000 == TYP_AURIF)
            {
              addStringToLabel("(Aurif - Composite)");
            }
            else if (Typ[i] == TYP_TABLE)
            {
              addStringToLabel("(Table)");
            }
            else if (Typ[i] / 50000000 * 50000000 == TYP_TABLE)
            {
              addStringToLabel("(Table - Composite)");
            }
            else if (Typ[i] == TYP_SIQS)
            {
              addStringToLabel("(SIQS)");
            }
            else if (Typ[i] / 50000000 * 50000000 == TYP_SIQS)
            {
              addStringToLabel("(SIQS - Composite)");
            }
            else if (Typ[i] == TYP_LEHMAN)
            {
              addStringToLabel("(Lehman)");
            }
            else if (Typ[i] / 50000000 * 50000000 == TYP_LEHMAN)
            {
              addStringToLabel("(Lehman - Composite)");
            }
            else if (Typ[i] > TYP_EC)
            {
              addStringToLabel("(Curve " + (Typ[i] - TYP_EC) + ")");
            }
            else
            {
              addStringToLabel("(Composite)");
            }
          }
          else if (
            Typ[i] < TYP_EC
              && Typ[i] != TYP_LEHMAN
              && Typ[i] != TYP_SIQS
              && Typ[i] != TYP_AURIF
              && Typ[i] != TYP_TABLE)
          {
            addStringToLabel("(Composite)");
          }
        }
        else
        {
          if (Typ[i] < 0)
          {
            addStringToLabel("(Unknown)");
          }
        }
      }
    }
    upperTextArea.setText(textAreaContents + StringToLabel);
  }

  void insertBigNbr(BigInteger N)
  {
    int i, dig;
    dig = digitsInGroup & 0x3FF;
    String value = N.toString();
    i = (value.length() + dig - 1) % dig + 1;
    addStringToLabel(value.substring(0, i));
    while (i < value.length())
    {
      addStringToLabel(value.substring(i, i + dig));
      i += dig;
    }
  }

  void InsertNewFactor(BigInteger InputFactor)
  {
    int g,exp;

    /* Insert input factor */
    for (g = NbrFactors - 1; g >= 0; g--)
    {
      PD[NbrFactors] = PD[g].gcd(InputFactor);
      if (PD[NbrFactors].equals(BigInt1) || PD[NbrFactors].equals(PD[g]))
      {
        continue;
      }
      for (exp=0; PD[g].remainder(PD[NbrFactors]).signum() == 0; exp++)
      {
        PD[g] = PD[g].divide(PD[NbrFactors]);
      }
      Exp[NbrFactors] = Exp[g] * exp;
      if (Typ[g] < 100000000)
      {
        Typ[g] = -EC;
        Typ[NbrFactors] = -TYP_EC - EC;
      }
      else if (Typ[g] < 150000000)
      {
        Typ[NbrFactors] = -Typ[g];
        Typ[g] = TYP_AURIF - Typ[g];
      }
      else if (Typ[g] < 200000000)
      {
        Typ[NbrFactors] = -Typ[g];
        Typ[g] = TYP_TABLE - Typ[g];
      }
      else if (Typ[g] < 250000000)
      {
        Typ[NbrFactors] = -Typ[g];
        Typ[g] = TYP_SIQS - Typ[g];
      }
      else
      {
        Typ[NbrFactors] = -Typ[g];
        Typ[g] = TYP_LEHMAN - Typ[g];
      }
      NbrFactors++;
    }
    SortFactorsInputNbr();
  }

  void SortFactorsInputNbr()
  {
    int g, i, j;
    BigInteger Nbr1;

    for (g = 0; g < NbrFactors - 1; g++)
    {
      for (j = g + 1; j < NbrFactors; j++)
      {
        if (PD[g].compareTo(PD[j]) > 0)
        {
          Nbr1 = PD[g];
          PD[g] = PD[j];
          PD[j] = Nbr1;
          i = Exp[g];
          Exp[g] = Exp[j];
          Exp[j] = i;
          i = Typ[g];
          Typ[g] = Typ[j];
          Typ[j] = i;
        }
      }
    }
  }

  void startNewFactorization(boolean completefactorization)
  {
    if (calcThread != null && batchFinished)
    {
      TerminateThread = true;
      try
      {
        calcThread.join(); /* Wait until the factorization thread dies */
      }
      catch (InterruptedException ie)
      {
      };
    }
    onlyFactoring = !completefactorization;
    lModularMult = OldTimeElapsed = NbrFactors = EC = 0;
    if (completefactorization)
    {
      factorize();
    }
    else
    {
      calcThread = new Thread(this); /* Start factorization thread */
      calcThread.start();
    }
  }

  public void run()
  {
    if (batchFinished == false)
    {
      BatchThread();
      return;
    }
    polynomialsSieved = 0;
    trialDivisions = 0;
    smoothsFound = 0;
    totalPartials = 0;
    partialsFound = 0;
    factorize();
  }

  void factorize()
  {
    BigInteger N, NN, OldPD;
    long TestComp, New;
    BigInteger N1, N2, Tmp;
    int i, j, k, OldExp;
    int ExpressionRC;
    BigInteger ExpressionResult[] = new BigInteger[1];

    StepECM = 0;
    primeModMult = 0;
    Computing3Squares = false;
    TerminateThread = false;
    Old = System.currentTimeMillis();
    if (onlyFactoring)
    {
      if (NbrFactors == 0)
      {
        lowerTextArea.setText("Computing input expression...");
        try
        {
          ExpressionRC =
            expression.ComputeExpression(
              textNumber.getText().trim(),
              0,
              ExpressionResult);
        }
        catch (OutOfMemoryError e)
        {
          lowerTextArea.setText("Out of memory.");
          return;
        }
        catch (ArithmeticException e)
        {
          return;
        }
        NumberToFactor = ExpressionResult[0];
        if (ExpressionRC != 0)
        {
          lowerTextArea.setText(expressionText[-1 - ExpressionRC]);
          return;
        }
      }
    }
    else
    {
      if (NbrFactors == 0)
      {
        NumberToFactor = new BigInteger(textNumber.getText().trim());
      }
    }
    BigNbr1[0] = 1;
    for (i = 1; i < NLen; i++)
    {
      BigNbr1[i] = 0;
    }
    try
    {
      if (NbrFactors == 0)
      {
        lowerTextArea.setText(
          "Searching for small factors (less than 131072).");
        TestComp = GetSmallFactors(NumberToFactor, PD, Exp, Typ, 0);
        if (TestComp != 1)
        {                      // There are factors greater than 131071.
          PD[NbrFactors] = BigIntToBigNbr(TestNbr);
          Exp[NbrFactors] = 1;
          Typ[NbrFactors] = -1; /* Unknown */
          NbrFactors++;
          ShowUpperPane();
          if (batchFinished || !batchPrime)
          {
            lowerTextArea.setText("Searching for perfect power plus/minus 1.");
            PowerPM1Check(); /* Find algebraic and Aurifeuillian factors */
            lowerTextArea.setText("Searching for Lucas number.");
            LucasCheck();
            lowerTextArea.setText("Searching for Fibonacci number.");
            FibonacciCheck();
          }
        }
      }
      performLehman = true;
      factor_loop : do
      {
        ShowUpperPane();
        for (i = 0; i < NbrFactors; i++)
        {
          if (Typ[i] < 0)
          { /* Unknown */
            lowerTextArea.setText("Searching for perfect power.");
            if (PowerCheck(i) != 0)
            {
              SortFactorsInputNbr();
              continue factor_loop;
            }
            if (PD[i].bitLength() <= 33)
            {
              j = 0;
            }
            else
            {
              lowerTextArea.setText("Before calling prime check routine.");
              long oldModularMult = lModularMult;
              j = AprtCle(PD[i]);
              primeModMult += lModularMult - oldModularMult;
              if (batchFinished == false && batchPrime)
              {
                NbrFactors = j;
                return;
              }
            }
            if (j == 0)
            {
              if (Typ[i] < -TYP_EC)
              {
                Typ[i] = -Typ[i]; /* Prime */
              }
              else if (Typ[i] < -TYP_LEHMAN)
              {
                Typ[i] = TYP_LEHMAN; /* Prime */
              }
              else if (Typ[i] < -TYP_SIQS)
              {
                Typ[i] = TYP_SIQS; /* Prime */
              }
              else if (Typ[i] < -TYP_AURIF)
              {
                Typ[i] = TYP_AURIF; /* Prime */
              }
              else
              {
                Typ[i] = 0; /* Prime */
              }
            }
            else
            {
              if (Typ[i] < -TYP_EC)
              {
                Typ[i] = -TYP_EC - Typ[i]; /* Composite */
              }
              else
              {
                Typ[i] = -Typ[i]; /* Composite */
              }
            }
            continue factor_loop;
          }
        }
        for (i = 0; i < NbrFactors; i++)
        {
          EC = Typ[i];
          if (EC > 0 && EC < TYP_EC && EC != TYP_AURIF
              && EC != TYP_SIQS && EC != TYP_LEHMAN)
          { /* Composite */
            EC %= 50000000;
            NN = fnECM(PD[i], i);
            if (NN.equals(BigInt1))
            {
              NN = FactoringSIQS(PD[i]);
            }
            if (foundByLehman)
            {              // Factor found using Lehman method
              Typ[i] = TYP_LEHMAN + EC + 1;
            }
            else
            {
              Typ[i] = EC;
            }
            InsertNewFactor(NN);
            continue factor_loop;
          }
        }
        break;
      }
      while (true);
      if (onlyFactoring)
      {
        textAreaContents = upperTextArea.getText() + "\n\n";
        StringToLabel = ""; // Start new line.
        addStringToLabel("Number of divisors: ");
        N1 = BigInt1;
        for (i = 0; i < NbrFactors; i++)
        {
          N1 = N1.multiply(BigInteger.valueOf(Exp[i] + 1));
        }
        insertBigNbr(N1); // Show number of divisors.
        textAreaContents += StringToLabel + "\n\n";
        StringToLabel = ""; // Start new line.
        addStringToLabel("Sum of divisors: ");
        N1 = BigInt1;
        for (i = 0; i < NbrFactors; i++)
        {
          N1 =
            N1.multiply(PD[i].pow(Exp[i] + 1).subtract(BigInt1)).divide(
              PD[i].subtract(BigInt1));
        }
        insertBigNbr(N1); // Show sum of divisors.
        textAreaContents += StringToLabel + "\n\n";
        StringToLabel = ""; // Start new line.
        addStringToLabel("Euler's Totient: ");
        N1 = NumberToFactor;
        for (i = 0; i < NbrFactors; i++)
        {
          N1 = N1.multiply(PD[i].subtract(BigInt1)).divide(PD[i]);
        }
        insertBigNbr(N1);
        j = 1; // Compute Moebius
        for (i = 0; i < NbrFactors; i++)
        {
          if (Exp[i] == 1)
          {
            j = -j;
          }
          else
          {
            j = 0;
          }
        } // Show Euler's totient and Moebius
        textAreaContents += StringToLabel + "\n\nMoebius: " + j;
        StringToLabel = "\n\nSum of squares: "; // Start new line.
        ComputeFourSquares(PD, Exp); // Quad1^2 + Quad2^2 + Quad3^2 + Quad4^2
        NbrFactors1 = NbrFactors;
        if (Quad4.signum() != 0)
        { // Check if four squares are really needed.
          j = NumberToFactor.getLowestSetBit();
          if (j % 2 != 0
            || NumberToFactor.shiftRight(j).and(BigInteger.valueOf(7)).equals(
              BigInteger.valueOf(7))
              == false)
          {
            /* Only three squares are required here */

            Computing3Squares = true;
            j = j / 2;
            lowerTextArea.setText("Computing sum of three squares...");
            for (N1 = BigInt1.shiftLeft(j);; N1 = N1.add(BigInt1.shiftLeft(j)))
            {
              if (TerminateThread)
              {
                throw new ArithmeticException();
              }
              New = System.currentTimeMillis();
              if (OldTimeElapsed >= 0
                && OldTimeElapsed / 1000 != (OldTimeElapsed + New - Old) / 1000)
              {
                OldTimeElapsed += New - Old;
                Old = New;
                labelStatus.setText(
                  "Time elapsed: "
                    + GetDHMS(OldTimeElapsed / 1000)
                    + "    mod mult: "
                    + (lModularMult >= 0 ? "" + lModularMult : "I don't know"));
              }
              N2 = NumberToFactor.subtract(N1.multiply(N1));
              TestComp = GetSmallFactors(N2, PD1, Exp1, Typ1, 1);
              if (TestComp >= 0)
              {
                if (TestComp == 1)
                { // Number has all factors < 2^17
                  ComputeFourSquares(PD1, Exp1); // Quad1^2 + Quad2^2
                  break;
                }
                if (TestNbr[0] % 4 == 3)
                {
                  continue;
                } // This value of c does not work
                PD1[NbrFactors] = BigIntToBigNbr(TestNbr);
                Exp1[NbrFactors] = 1;
                NbrFactors++;
                if (ComputeFourSquares(PD1, Exp1))
                { // Quad1^2 + Quad2^2
                  break;
                }
              }
            } /* end for */
            Quad3 = N1;
            // Sort squares (only Quad3 can be out of order).
            if (Quad1.compareTo(Quad3) < 0)
            {
              Tmp = Quad1;
              Quad1 = Quad3;
              Quad3 = Tmp;
            }
            if (Quad2.compareTo(Quad3) < 0)
            {
              Tmp = Quad2;
              Quad2 = Quad3;
              Quad3 = Tmp;
            }
            Computing3Squares = false;
          }
        }
        NbrFactors = NbrFactors1;
        if (Quad4.signum() == 0)
        {
          if (Quad3.signum() == 0)
          {
            if (Quad2.signum() == 0)
            {
              insertBigNbr(Quad1);
              addStringToLabel("^2");
            }
            else
            {
              textAreaContents += StringToLabel + "a^2 + b^2\n";
              StringToLabel = "a = "; // Start new line.
              insertBigNbr(Quad1);
              textAreaContents += StringToLabel + "\n";
              StringToLabel = "b = "; // Start new line.
              insertBigNbr(Quad2);
            }
          }
          else
          {
            textAreaContents += StringToLabel + "a^2 + b^2 + c^2\n";
            StringToLabel = "a = "; // Start new line.
            insertBigNbr(Quad1);
            textAreaContents += StringToLabel + "\n";
            StringToLabel = "b = "; // Start new line.
            insertBigNbr(Quad2);
            textAreaContents += StringToLabel + "\n";
            StringToLabel = "c = "; // Start new line.
            insertBigNbr(Quad3);
          }
        }
        else
        {
          textAreaContents += StringToLabel + "a^2 + b^2 + c^2 + d^2\n";
          StringToLabel = "a = "; // Start new line.
          insertBigNbr(Quad1);
          textAreaContents += StringToLabel + "\n";
          StringToLabel = "b = "; // Start new line.
          insertBigNbr(Quad2);
          textAreaContents += StringToLabel + "\n";
          StringToLabel = "c = "; // Start new line.
          insertBigNbr(Quad3);
          textAreaContents += StringToLabel + "\n";
          StringToLabel = "d = "; // Start new line.
          insertBigNbr(Quad4);
        }
        upperTextArea.setText(textAreaContents + StringToLabel);
        New = System.currentTimeMillis();
        if (OldTimeElapsed >= 0)
        {
          OldTimeElapsed += New - Old;
          String timeElapsed = GetDHMS(OldTimeElapsed / 1000);
          String textAreaInfo = "Factorization complete in " + timeElapsed;
          if (lModularMult >= 0)
          {
            textAreaInfo += "\nECM: "
              + (lModularMult - primeModMult)
              + " modular multiplications\nPrime checking: "
              + primeModMult
              + " modular multiplications";
          }
          if (smoothsFound > 0)
          {
            textAreaInfo += "\nSIQS: "
              + polynomialsSieved
              + " polynomials sieved\n      "
              + trialDivisions
              + " sets of trial divisions\n      "
              + smoothsFound
              + " smooth congruences found (1 out of every "
              + ValuesSieved/smoothsFound+" values)\n      "
              + totalPartials
              + " partial congruences found (1 out of every "
              + ValuesSieved/totalPartials+" values)\n      "
              + partialsFound
              + " useful partial congruences";
          }
          lowerTextArea.setText(textAreaInfo);
          labelStatus.setText(
            "Time elapsed: "
              + timeElapsed
              + "    mod mult: "
              + (lModularMult >= 0 ? "" + lModularMult : "I don't know"));
        }
        else
        {
          lowerTextArea.setText("Factorization complete");
        }
        NextEC = -1; /* First curve of new number should be 1 */
      }
    }
    catch (ArithmeticException e)
    {
      New = System.currentTimeMillis();
      if (OldTimeElapsed >= 0)
      {
        OldTimeElapsed += New - Old;
      }
      System.gc();
      return;
    }
    System.gc();
  }

  long GetSmallFactors(
    BigInteger NumberToFactor,
    BigInteger PD[],
    int Exp[],
    int Typ[],
    int Type)
  {

    long Div, TestComp;
    int i;
    boolean checkExpParity = false;

    BigNbrToBigInt(NumberToFactor);
    NbrFactors = 0;
    for (i = 0; i < 400; i++)
    {
      Exp[i] = Typ[i] = 0;
    }
    while ((TestNbr[0] & 1) == 0)
    { /* N even */
      if (Exp[NbrFactors] == 0)
      {
        PD[NbrFactors] = BigInt2;
      }
      Exp[NbrFactors]++;
      DivBigNbrByLong(TestNbr, 2, TestNbr);
    }
    if (Exp[NbrFactors] != 0)
    {
      NbrFactors++;
    }
    while (RemDivBigNbrByLong(TestNbr, 3) == 0)
    {
      if (Type == 1)
      {
        checkExpParity = !checkExpParity;
      }
      if (Exp[NbrFactors] == 0)
      {
        PD[NbrFactors] = BigInt3;
      }
      Exp[NbrFactors]++;
      DivBigNbrByLong(TestNbr, 3, TestNbr);
    }
    if (checkExpParity)
    {
      return -1; /* Discard it */
    }
    if (Exp[NbrFactors] != 0)
    {
      NbrFactors++;
    }
    Div = 5;
    TestComp = TestNbr[0] + (TestNbr[1] << 31);
    if (TestComp < 0)
    {
      TestComp = 10000 * DosALa31;
    }
    else
    {
      for (i = 2; i < NumberLength; i++)
      {
        if (TestNbr[i] != 0)
        {
          TestComp = 10000 * DosALa31;
          break;
        }
      }
    }
    while (Div < 131072)
    {
      if (Div % 3 != 0)
      {
        while (RemDivBigNbrByLong(TestNbr, Div) == 0)
        {
          if (Type == 1 && Div % 4 == 3)
          {
            checkExpParity = !checkExpParity;
          }
          if (Exp[NbrFactors] == 0)
          {
            PD[NbrFactors] = BigInteger.valueOf(Div);
          }
          Exp[NbrFactors]++;
          DivBigNbrByLong(TestNbr, Div, TestNbr);
          TestComp = TestNbr[0] + (TestNbr[1] << 31);
          if (TestComp < 0)
          {
            TestComp = 10000 * DosALa31;
          }
          else
          {
            for (i = 2; i < NumberLength; i++)
            {
              if (TestNbr[i] != 0)
              {
                TestComp = 10000 * DosALa31;
                break;
              }
            }
          } /* end while */
        }
        if (checkExpParity)
        {
          return -1; /* Discard it */
        }
        if (Exp[NbrFactors] != 0)
        {
          NbrFactors++;
        }
      }
      Div += 2;
      if (TestComp < Div * Div && TestComp != 1)
      {
        if (Type == 1 && TestComp % 4 == 3)
        {
          return -1; /* Discard it */
        }
        if (Exp[NbrFactors] != 0)
        {
          NbrFactors++;
        }
        PD[NbrFactors] = BigInteger.valueOf(TestComp);
        Exp[NbrFactors] = 1;
        TestComp = 1;
        NbrFactors++;
        break;
      }
    } /* end while */
    return TestComp;
  }

  int PowerCheck(int i)
  {
    long New;
    int maxExpon = (PD[i].bitLength() - 1) / 17;
    int comp, h, j;
    long modulus, modulus2;
    int intLog2N;
    double log2N;
    BigInteger root, rootN1, rootN, dif, nextroot;
    int prime2310x1[] =
      { 2311, 4621, 9241, 11551, 18481, 25411, 32341, 34651, 43891, 50821 };
    // Primes of the form 2310x+1.
    boolean expon2 = true, expon3 = true, expon5 = true;
    boolean expon7 = true, expon11 = true;
    for (h = 0; h < prime2310x1.length; h++)
    {
      long testprime = prime2310x1[h];
      long mod = PD[i].mod(BigInteger.valueOf(testprime)).intValue();
      if (expon2 && modPow(mod, testprime / 2, testprime) > 1)
        expon2 = false;
      if (expon3 && modPow(mod, testprime / 3, testprime) > 1)
        expon3 = false;
      if (expon5 && modPow(mod, testprime / 5, testprime) > 1)
        expon5 = false;
      if (expon7 && modPow(mod, testprime / 7, testprime) > 1)
        expon7 = false;
      if (expon11 && modPow(mod, testprime / 11, testprime) > 1)
        expon11 = false;
    }
    boolean ProcessExpon[] = new boolean[maxExpon + 1];
    boolean primes[] = new boolean[2 * maxExpon + 3];
    for (h = 2; h <= maxExpon; h++)
    {
      ProcessExpon[h] = true;
    }
    for (h = 2; h < primes.length; h++)
    {
      primes[h] = true;
    }
    for (h = 2; h * h < primes.length; h++)
    { // Generation of primes
      for (j = h * h; j < primes.length; j += h)
      { // using Eratosthenes sieve
        primes[j] = false;
      }
    }
    for (h = 13; h < primes.length; h++)
    {
      if (primes[h])
      {
        int processed = 0;
        for (j = 2 * h + 1; j < primes.length; j += 2 * h)
        {
          if (primes[j])
          {
            modulus = PD[i].mod(BigInteger.valueOf(j)).longValue();
            if (modPow(modulus, j / h, j) > 1)
            {
              for (j = h; j <= maxExpon; j += h)
              {
                ProcessExpon[j] = false;
              }
              break;
            }
          }
          if (++processed > 10)
            break;
        }
      }
    }
    for (int Exponent = maxExpon; Exponent >= 2; Exponent--)
    {
      if (Exponent % 2 == 0 && expon2 == false)
        continue; // Not a square
      if (Exponent % 3 == 0 && expon3 == false)
        continue; // Not a cube
      if (Exponent % 5 == 0 && expon5 == false)
        continue; // Not a fifth power
      if (Exponent % 7 == 0 && expon7 == false)
        continue; // Not a 7th power
      if (Exponent % 11 == 0 && expon11 == false)
        continue; // Not an 11th power
      if (ProcessExpon[Exponent] == false)
        continue;
      New = System.currentTimeMillis();
      if (OldTimeElapsed >= 0
        && OldTimeElapsed / 1000 != (OldTimeElapsed + New - Old) / 1000)
      {
        OldTimeElapsed += New - Old;
        Old = New;
        labelStatus.setText(
          "Time elapsed: "
            + GetDHMS(OldTimeElapsed / 1000)
            + "    Power exponent: "
            + Exponent);
        Thread.yield();
        if (TerminateThread)
        {
          throw new ArithmeticException();
        }
      }
      intLog2N = PD[i].bitLength() - 1;
      log2N =
        intLog2N
          + Math.log(PD[i].shiftRight(intLog2N - 32).add(BigInt1).doubleValue())
            / Math.log(2)
          - 32;
      log2N /= Exponent;
      if (log2N < 32)
      {
        root = BigInteger.valueOf((long) Math.exp(log2N * Math.log(2)));
      }
      else
      {
        intLog2N = (int) Math.floor(log2N) - 32;
        root =
          BigInteger
            .valueOf((long) Math.exp((log2N - intLog2N) * Math.log(2)) + 10)
            .shiftLeft(intLog2N);
      }
      while (true)
      {
        rootN1 = root.pow(Exponent - 1);
        rootN = root.multiply(rootN1);
        dif = PD[i].subtract(rootN);
        if (dif.signum() == 0)
        { // Perfect power
          PD[i] = root;
          Exp[i] *= Exponent;
          return 1;
        }
        nextroot =
          dif
            .add(BigInt1)
            .divide(BigInteger.valueOf(Exponent).multiply(rootN1))
            .add(root)
            .subtract(BigInt1);
        if (root.compareTo(nextroot) <= 0)
          break; // Not a perfect power
        root = nextroot;
      }
    }
    return 0;
  }

  // Perform Lehman algorithm
  static BigInteger Lehman(BigInteger nbr, int k)
  {
    long bitsSqr[] = { 0x0000000000000003l, // 3
      0x0000000000000013l, // 5
      0x0000000000000017l, // 7
      0x000000000000023Bl, // 11
      0x000000000000161Bl, // 13
      0x000000000001A317l, // 17
      0x0000000000030AF3l, // 19
      0x000000000005335Fl, // 23
      0x0000000013D122F3l, // 29
      0x00000000121D47B7l, // 31
      0x000000165E211E9Bl, // 37
      0x000001B382B50737l, // 41
      0x0000035883A3EE53l, // 43
      0x000004351B2753DFl, // 47
      0x0012DD703303AED3l, // 53
      0x022B62183E7B92BBl, // 59
      0x1713E6940A59F23Bl, // 61
    };
    int primes[] =
      { 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61 };
    int nbrs[] = new int[17];
    int diffs[] = new int[17];
    int i, j, m;
    int intLog2N;
    double log2N;
    BigInteger root, rootN1, rootN, dif, nextroot;
    BigInteger bM, a, c, r, sqr, val;
    if (nbr.testBit(0) == false)
    { // nbr Even
      r = BigInt0;
      m = 1;
      bM = BigInt1;
    }
    else
    {
      if (k % 2 == 0)
      { // k Even
        r = BigInt1;
        m = 2;
        bM = BigInt2;
      }
      else
      { // k Odd
        r = BigInteger.valueOf(k).add(nbr).and(BigInt3);
        m = 4;
        bM = BigInteger.valueOf(4);
      }
    }
    sqr = nbr.multiply(BigInteger.valueOf(k)).shiftLeft(2);
    intLog2N = sqr.bitLength() - 1;
    log2N =
      intLog2N
        + Math.log(sqr.shiftRight(intLog2N - 32).add(BigInt1).doubleValue())
          / Math.log(2)
        - 32;
    log2N /= 2;
    if (log2N < 32)
    {
      root = BigInteger.valueOf((long) Math.exp(log2N * Math.log(2)));
    }
    else
    {
      intLog2N = (int) Math.floor(log2N) - 32;
      root =
        BigInteger
          .valueOf((long) Math.exp((log2N - intLog2N) * Math.log(2)) + 10)
          .shiftLeft(intLog2N);
    }
    while (true)
    {
      rootN = root.multiply(root);
      dif = sqr.subtract(rootN);
      if (dif.signum() == 0)
      { // Perfect power
        break;
      }
      nextroot =
        dif.add(BigInt1).divide(BigInt2.multiply(root)).add(root).subtract(
          BigInt1);
      if (root.compareTo(nextroot) <= 0)
        break; // Not a perfect power
      root = nextroot;
    }
    a = root;
    while (a.mod(bM).equals(r) == false ||
           a.multiply(a).compareTo(sqr)<0)
    {
      a = a.add(BigInt1);
    }
    c = a.multiply(a).subtract(sqr);
    for (i = 0; i < 17; i++)
    {
      BigInteger prime = BigInteger.valueOf(primes[i]);
      nbrs[i] = c.mod(prime).intValue();
      diffs[i] = bM.multiply(a.shiftLeft(1).add(bM)).mod(prime).intValue();
    }
    for (j = 0; j < 10000; j++)
    {
      for (i = 0; i < 17; i++)
      {
        if ((bitsSqr[i] & (1l << nbrs[i])) == 0)
        { // Not a perfect square
          break;
        }
      }
      if (i == 17)
      { // Test for perfect square
        val = a.add(BigInteger.valueOf(m * j));
        c = val.multiply(val).subtract(sqr);
        intLog2N = c.bitLength() - 1;
        log2N =
          intLog2N
            + Math.log(c.shiftRight(intLog2N - 32).add(BigInt1).doubleValue())
              / Math.log(2)
            - 32;
        log2N /= 2;
        if (log2N < 32)
        {
          root = BigInteger.valueOf((long) Math.exp(log2N * Math.log(2)));
        }
        else
        {
          intLog2N = (int) Math.floor(log2N) - 32;
          root =
            BigInteger
              .valueOf((long) Math.exp((log2N - intLog2N) * Math.log(2)) + 10)
              .shiftLeft(intLog2N);
        }
        while (true)
        {
          rootN = root.multiply(root);
          dif = c.subtract(rootN);
          if (dif.signum() == 0)
          { // Perfect power -> factor found
            root = nbr.gcd(val.add(root));
            if (root.compareTo(BigInteger.valueOf(10000)) > 0)
            {
              return root; // Return non-trivial found
            }
          }
          nextroot =
            dif.add(BigInt1).divide(BigInt2.multiply(root)).add(root).subtract(
              BigInt1);
          if (root.compareTo(nextroot) <= 0)
            break; // Not a perfect power
          root = nextroot;
        }
      }
      for (i = 0; i < 17; i++)
      {
        nbrs[i] = (nbrs[i] + diffs[i]) % primes[i];
        diffs[i] = (diffs[i] + 2 * m * m) % primes[i];
      }
    }
    return BigInt1; // Factor not found
  }

  final void PowerPM1Check()
  {
    if (onlyFactoring)
    {
      boolean plus1 = false;
      boolean minus1 = false;
      int Exponent = 0;
      int comp, i, j;
      int modulus;
      long i2;
      int mod9 = NumberToFactor.mod(BigInteger.valueOf(9L)).intValue();
      int maxExpon = NumberToFactor.bitLength();
      double logar =
        (maxExpon - 32) * Math.log(2)
          + Math.log(NumberToFactor.shiftRight(maxExpon - 32).longValue());
      boolean ProcessExpon[] = new boolean[maxExpon + 1];
      boolean primes[] = new boolean[2 * maxExpon + 3];
      for (i = 2; i <= maxExpon; i++)
      {
        ProcessExpon[i] = true;
      }
      for (i = 2; i < primes.length; i++)
      {
        primes[i] = true;
      }
      for (i = 2; i * i < primes.length; i++)
      { // Generation of primes
        for (j = i * i; j < primes.length; j += i)
        {
          primes[j] = false;
        }
      }
      // If the number +/- 1 is multiple of a prime but not a multiple
      // of its square then the number +/- 1 cannot be a perfect power.
      for (i = 2; i < primes.length; i++)
      {
        if (primes[i])
        {
          i2 = (long) i * (long) i;
          modulus = NumberToFactor.mod(BigInteger.valueOf(i)).intValue();
          if (modulus == 1
            && NumberToFactor.mod(BigInteger.valueOf(i2)).longValue() != 1L)
          {
            plus1 = true; // NumberFactor cannot be a power + 1
          }
          if (modulus == i - 1
            && NumberToFactor.mod(BigInteger.valueOf(i2)).longValue() != i2 - 1L)
          {
            minus1 = true; // NumberFactor cannot be a power - 1
          }
          if (minus1 && plus1)
            return;
          if (ProcessExpon[i / 2] == false)
          {
            continue;
          }
          if (modulus > (plus1 ? 1 : 2) && modulus < (minus1 ? i - 1 : i - 2))
          {
            for (j = i / 2; j <= maxExpon; j += i / 2)
            {
              ProcessExpon[j] = false;
            }
          }
          else
          {
            if (modulus == i - 2)
            {
              for (j = i - 1; j <= maxExpon; j += i - 1)
              {
                ProcessExpon[j] = false;
              }
            }
          }
        }
      }
      for (j = 2; j < 100; j++)
      {
        double u = logar / Math.log(j) + .000005;
        Exponent = (int) Math.floor(u);
        if (u - Exponent > .00001)
          continue;
        if (Exponent % 3 == 0 && mod9 > 2 && mod9 < 7)
          continue;
        if (ProcessExpon[Exponent] == false)
          continue;
        if (ProcessExponent(Exponent))
          return;
      }
      for (; Exponent >= 2; Exponent--)
      {
        if (Exponent % 3 == 0 && mod9 > 2 && mod9 < 7)
          continue;
        if (ProcessExpon[Exponent] == false)
          continue;
        if (ProcessExponent(Exponent))
          return;
      }
    }
  }

  private boolean ProcessExponent(int Exponent)
  {
    BigInteger NFp1, NFm1, root, rootN1, rootN, rootbak;
    BigInteger nextroot, dif;
    int intLog2N;
    double log2N;
    long New = System.currentTimeMillis();
    if (OldTimeElapsed >= 0
      && OldTimeElapsed / 1000 != (OldTimeElapsed + New - Old) / 1000)
    {
      OldTimeElapsed += New - Old;
      Old = New;
      labelStatus.setText(
        "Time elapsed: "
          + GetDHMS(OldTimeElapsed / 1000)
          + "    Power +/- 1 exponent: "
          + Exponent);
      Thread.yield();
      if (TerminateThread)
      {
        throw new ArithmeticException();
      }
    }
    NFp1 = NumberToFactor.add(BigInt1);
    NFm1 = NumberToFactor.subtract(BigInt1);
    intLog2N = NFp1.bitLength() - 1;
    log2N =
      intLog2N
        + Math.log(NFp1.shiftRight(intLog2N - 32).add(BigInt1).doubleValue())
          / Math.log(2)
        - 32;
    log2N /= Exponent;
    if (log2N < 32)
    {
      root = BigInteger.valueOf((long) Math.exp(log2N * Math.log(2)));
    }
    else
    {
      intLog2N = (int) Math.floor(log2N) - 32;
      root =
        BigInteger
          .valueOf((long) Math.exp((log2N - intLog2N) * Math.log(2)) + 10)
          .shiftLeft(intLog2N);
    }
    rootbak = root;
    while (true)
    {
      rootN1 = root.pow(Exponent - 1);
      rootN = root.multiply(rootN1);
      dif = NFp1.subtract(rootN);
      if (dif.signum() == 0)
      { // Perfect power
        Cunningham(root, Exponent, BigInt1.negate(), PD[NbrFactors - 1]);
        return true;
      }
      nextroot =
        dif
          .add(BigInt1)
          .divide(BigInteger.valueOf(Exponent).multiply(rootN1))
          .add(root)
          .subtract(BigInt1);
      if (root.compareTo(nextroot) <= 0)
        break; // Not a perfect power
      root = nextroot;
    }
    root = rootbak;
    while (true)
    {
      rootN1 = root.pow(Exponent - 1);
      rootN = root.multiply(rootN1);
      dif = NFm1.subtract(rootN);
      if (dif.signum() == 0)
      { // Perfect power
        Cunningham(root, Exponent, BigInt1, PD[NbrFactors - 1]);
        return true;
      }
      nextroot =
        dif
          .add(BigInt1)
          .divide(BigInteger.valueOf(Exponent).multiply(rootN1))
          .add(root)
          .subtract(BigInt1);
      if (root.compareTo(nextroot) <= 0)
        break; // Not a perfect power
      root = nextroot;
    }
    return false;
  }

  final void LucasCheck()
  {
    int i, j;
    if (onlyFactoring)
    {
      if (NumberToFactor.bitLength() > 32)
      {
        int maxExpon = NumberToFactor.bitLength();
        double logar =
          (maxExpon - 32) * Math.log(2)
            + Math.log(NumberToFactor.shiftRight(maxExpon - 32).longValue());
        logar = logar / 0.481211825059603; // index of L
        if (logar + .000005 - Math.floor(logar + .000005) > .00001)
          return;
      }
      BigInteger LucasPrev = BigInteger.valueOf(-1);
      BigInteger LucasAct = BigInt2;
      BigInteger LucasNext;
      i = 0;
      while (true)
      {
        j = LucasAct.compareTo(NumberToFactor);
        if (j == 0)
        {
          FactorLucas(i, PD[NbrFactors - 1]);
          return;
        }
        if (j > 0)
        {
          return;
        }
        LucasNext = LucasPrev.add(LucasAct);
        LucasPrev = LucasAct;
        LucasAct = LucasNext;
        i++;
      }
    }
  }

  final void FibonacciCheck()
  {
    int i, j;
    if (onlyFactoring)
    {
      if (NumberToFactor.bitLength() > 32)
      {
        int maxExpon = NumberToFactor.bitLength();
        double logar =
          (maxExpon - 32) * Math.log(2)
            + Math.log(NumberToFactor.shiftRight(maxExpon - 32).longValue());
        logar = (logar + 0.80471895621705) / 0.481211825059603; // index of F.
        if (logar + .000005 - Math.floor(logar + .000005) > .00001)
          return;
      }
      BigInteger FibonPrev = BigInt1;
      BigInteger FibonAct = BigInt0;
      BigInteger FibonNext;
      i = 0;
      while (true)
      {
        j = FibonAct.compareTo(NumberToFactor);
        if (j == 0)
        {
          FactorFibonacci(i, PD[NbrFactors - 1]);
          return;
        }
        if (j > 0)
        {
          return;
        }
        FibonNext = FibonPrev.add(FibonAct);
        FibonPrev = FibonAct;
        FibonAct = FibonNext;
        i++;
      }
    }
  }

  // Prime checking routine
  // Return codes: 0 = Number is prime.
  //               1 = Number is composite.
  int AprtCle(BigInteger N)
  {
    int i, j, G, H, I, J, K, P, Q, T, U, W, X;
    int IV, InvX, LEVELnow, NP, PK, PL, PM, SW, VK, TestedQs, TestingQs;
    int QQ, T1, T3, U1, U3, V1, V3;
    int LengthN, LengthS;
    long Mask;
    double dS;
    String primalityString = "";

    lowerTextArea.setText("Starting Prime Check routine.");
    BigNbrToBigInt(N);
    GetMontgomeryParms();
    if (Computing3Squares == false)
    {
      textAreaContents = "";
      StringToLabel = "Testing primality of ";
      insertBigNbr(N);
      addStringToLabel("(" + N.toString().length() + " digits)");
      primalityString =
        textAreaContents + StringToLabel + "\nAPRT-CLE progress: ";
    }
    j = PK = PL = PM = 0;
    for (I = 0; I < NumberLength; I++)
    {
      biS[I] = 0;
      for (J = 0; J < PWmax; J++)
      {
        aiJX[J][I] = 0;
      }
    }
    GetPrimes2Test : for (i = 0; i < LEVELmax; i++)
    {
      biS[0] = 2;
      for (I = 1; I < NumberLength; I++)
      {
        biS[I] = 0;
      }
      for (j = 0; j < aiNQ[i]; j++)
      {
        Q = aiQ[j];
        U = aiT[i] * Q;
        do
        {
          U /= Q;
          MultBigNbrByLong(biS, Q, biS);
        }
        while (U % Q == 0);

        // Exit loop if S^2 > N.

        if (CompareSquare(biS, TestNbr) > 0)
        {
          break GetPrimes2Test;
        }
      } /* End for j */
    } /* End for i */
    if (i == LEVELmax)
    { /* too big */
      return ProbabilisticPrimeTest(N);
    }
    LEVELnow = i;
    TestingQs = j;
    T = aiT[LEVELnow];
    NP = aiNP[LEVELnow];

    MainStart : do
    {
      for (i = 0; i < NP; i++)
      {
        P = aiP[i];
        SW = TestedQs = 0;
        Q = W = (int) BigNbrModLong(TestNbr, P * P);
        for (J = P - 2; J > 0; J--)
        {
          W = (W * Q) % (P * P);
        }
        if (P > 2 && W != 1)
        {
          SW = 1;
        }
        do
        {
          for (j = TestedQs; j <= TestingQs; j++)
          {
            Q = aiQ[j] - 1;
            G = aiG[j];
            K = 0;
            while (Q % P == 0)
            {
              K++;
              Q /= P;
            }
            Q = aiQ[j];
            if (K == 0)
            {
              continue;
            }
            if (Computing3Squares == false)
            {
              lowerTextArea.setText(
                primalityString
                  + "P = "
                  + P
                  + ",  Q = "
                  + Q
                  + "  ("
                  + (i * (TestingQs + 1) + j) * 100 / (NP * (TestingQs + 1))
                  + "%)");
            }
            PM = 1;
            for (I = 1; I < K; I++)
            {
              PM = PM * P;
            }
            PL = (P - 1) * PM;
            PK = P * PM;
            J = 1;
            for (I = 1; I < Q; I++)
            {
              J = J * G % Q;
              aiIndx[J] = I;
            }
            J = 1;
            for (I = 1; I <= Q - 2; I++)
            {
              J = J * G % Q;
              aiF[I] = aiIndx[(Q + 1 - J) % Q];
            }
            for (I = 0; I < PK; I++)
            {
              for (J = 0; J < NumberLength; J++)
              {
                aiJ0[I][J] = aiJ1[I][J] = 0;
              }
            }
            if (P > 2)
            {
              JacobiSum(1, 1, P, PK, PL, PM, Q);
            }
            else
            {
              if (K != 1)
              {
                JacobiSum(1, 1, P, PK, PL, PM, Q);
                for (I = 0; I < PK; I++)
                {
                  for (J = 0; J < NumberLength; J++)
                  {
                    aiJW[I][J] = 0;
                  }
                }
                if (K != 2)
                {
                  for (I = 0; I < PM; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJW[I][J] = aiJ0[I][J];
                    }
                  }
                  JacobiSum(2, 1, P, PK, PL, PM, Q);
                  for (I = 0; I < PM; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJS[I][J] = aiJ0[I][J];
                    }
                  }
                  JS_JW(PK, PL, PM, P);
                  NormalizeJS(PK, PL, PM, P);
                  for (I = 0; I < PM; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJ1[I][J] = aiJS[I][J];
                    }
                  }
                  JacobiSum(3 << (K - 3), 1 << (K - 3), P, PK, PL, PM, Q);
                  for (J = 0; J < NumberLength; J++)
                  {
                    for (I = 0; I < PK; I++)
                    {
                      aiJW[I][J] = 0;
                    }
                    for (I = 0; I < PM; I++)
                    {
                      aiJS[I][J] = aiJ0[I][J];
                    }
                  }
                  JS_2(PK, PL, PM, P);
                  NormalizeJS(PK, PL, PM, P);
                  for (I = 0; I < PM; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJ2[I][J] = aiJS[I][J];
                    }
                  }
                }
              }
            }
            for (J = 0; J < NumberLength; J++)
            {
              aiJ00[0][J] = aiJ01[0][J] = MontgomeryMultR1[J];
              for (I = 1; I < PK; I++)
              {
                aiJ00[I][J] = aiJ01[I][J] = 0;
              }
            }
            VK = (int) BigNbrModLong(TestNbr, PK);
            for (I = 1; I < PK; I++)
            {
              if (I % P != 0)
              {
                U1 = 1;
                U3 = I;
                V1 = 0;
                V3 = PK;
                while (V3 != 0)
                {
                  QQ = U3 / V3;
                  T1 = U1 - V1 * QQ;
                  T3 = U3 - V3 * QQ;
                  U1 = V1;
                  U3 = V3;
                  V1 = T1;
                  V3 = T3;
                }
                aiInv[I] = (U1 + PK) % PK;
              }
              else
              {
                aiInv[I] = 0;
              }
            }
            if (P != 2)
            {
              for (IV = 0; IV <= 1; IV++)
              {
                for (X = 1; X < PK; X++)
                {
                  for (I = 0; I < PK; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJS[I][J] = aiJ0[I][J];
                    }
                  }
                  if (X % P == 0)
                  {
                    continue;
                  }
                  if (IV == 0)
                  {
                    LongToBigNbr(X, biExp);
                  }
                  else
                  {
                    LongToBigNbr(VK * X / PK, biExp);
                    if (VK * X / PK == 0)
                    {
                      continue;
                    }
                  }
                  JS_E(PK, PL, PM, P);
                  for (I = 0; I < PK; I++)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJW[I][J] = 0;
                    }
                  }
                  InvX = aiInv[X];
                  for (I = 0; I < PK; I++)
                  {
                    J = I * InvX % PK;
                    AddBigNbrModN(aiJW[J], aiJS[I], aiJW[J]);
                  }
                  NormalizeJW(PK, PL, PM, P);
                  if (IV == 0)
                  {
                    for (I = 0; I < PK; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJS[I][J] = aiJ00[I][J];
                      }
                    }
                  }
                  else
                  {
                    for (I = 0; I < PK; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJS[I][J] = aiJ01[I][J];
                      }
                    }
                  }
                  JS_JW(PK, PL, PM, P);
                  if (IV == 0)
                  {
                    for (I = 0; I < PK; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJ00[I][J] = aiJS[I][J];
                      }
                    }
                  }
                  else
                  {
                    for (I = 0; I < PK; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJ01[I][J] = aiJS[I][J];
                      }
                    }
                  }
                } /* end for X */
              } /* end for IV */
            }
            else
            {
              if (K == 1)
              {
                MultBigNbrByLongModN(MontgomeryMultR1, Q, aiJ00[0]);
                for (J = 0; J < NumberLength; J++)
                {
                  aiJ01[0][J] = MontgomeryMultR1[J];
                }
              }
              else
              {
                if (K == 2)
                {
                  if (VK == 1)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJ01[0][J] = MontgomeryMultR1[J];
                    }
                  }
                  for (J = 0; J < NumberLength; J++)
                  {
                    aiJS[0][J] = aiJ0[0][J];
                    aiJS[1][J] = aiJ0[1][J];
                  }
                  JS_2(PK, PL, PM, P);
                  if (VK == 3)
                  {
                    for (J = 0; J < NumberLength; J++)
                    {
                      aiJ01[0][J] = aiJS[0][J];
                      aiJ01[1][J] = aiJS[1][J];
                    }
                  }
                  MultBigNbrByLongModN(aiJS[0], Q, aiJ00[0]);
                  MultBigNbrByLongModN(aiJS[1], Q, aiJ00[1]);
                }
                else
                {
                  for (IV = 0; IV <= 1; IV++)
                  {
                    for (X = 1; X < PK; X += 2)
                    {
                      for (I = 0; I <= PM; I++)
                      {
                        for (J = 0; J < NumberLength; J++)
                        {
                          aiJS[I][J] = aiJ1[I][J];
                        }
                      }
                      if (X % 8 == 5 || X % 8 == 7)
                      {
                        continue;
                      }
                      if (IV == 0)
                      {
                        LongToBigNbr(X, biExp);
                      }
                      else
                      {
                        LongToBigNbr(VK * X / PK, biExp);
                        if (VK * X / PK == 0)
                        {
                          continue;
                        }
                      }
                      JS_E(PK, PL, PM, P);
                      for (I = 0; I < PK; I++)
                      {
                        for (J = 0; J < NumberLength; J++)
                        {
                          aiJW[I][J] = 0;
                        }
                      }
                      InvX = aiInv[X];
                      for (I = 0; I < PK; I++)
                      {
                        J = I * InvX % PK;
                        AddBigNbrModN(aiJW[J], aiJS[I], aiJW[J]);
                      }
                      NormalizeJW(PK, PL, PM, P);
                      if (IV == 0)
                      {
                        for (I = 0; I < PK; I++)
                        {
                          for (J = 0; J < NumberLength; J++)
                          {
                            aiJS[I][J] = aiJ00[I][J];
                          }
                        }
                      }
                      else
                      {
                        for (I = 0; I < PK; I++)
                        {
                          for (J = 0; J < NumberLength; J++)
                          {
                            aiJS[I][J] = aiJ01[I][J];
                          }
                        }
                      }
                      NormalizeJS(PK, PL, PM, P);
                      JS_JW(PK, PL, PM, P);
                      if (IV == 0)
                      {
                        for (I = 0; I < PK; I++)
                        {
                          for (J = 0; J < NumberLength; J++)
                          {
                            aiJ00[I][J] = aiJS[I][J];
                          }
                        }
                      }
                      else
                      {
                        for (I = 0; I < PK; I++)
                        {
                          for (J = 0; J < NumberLength; J++)
                          {
                            aiJ01[I][J] = aiJS[I][J];
                          }
                        }
                      }
                    } /* end for X */
                    if (IV == 0 || VK % 8 == 1 || VK % 8 == 3)
                    {
                      continue;
                    }
                    for (I = 0; I < PM; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJW[I][J] = aiJ2[I][J];
                        aiJS[I][J] = aiJ01[I][J];
                      }
                    }
                    for (; I < PK; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJW[I][J] = aiJS[I][J] = 0;
                      }
                    }
                    JS_JW(PK, PL, PM, P);
                    for (I = 0; I < PM; I++)
                    {
                      for (J = 0; J < NumberLength; J++)
                      {
                        aiJ01[I][J] = aiJS[I][J];
                      }
                    }
                  } /* end for IV */
                }
              }
            }
            for (I = 0; I < PL; I++)
            {
              for (J = 0; J < NumberLength; J++)
              {
                aiJS[I][J] = aiJ00[I][J];
              }
            }
            for (; I < PK; I++)
            {
              for (J = 0; J < NumberLength; J++)
              {
                aiJS[I][J] = 0;
              }
            }
            DivBigNbrByLong(TestNbr, PK, biExp);
            JS_E(PK, PL, PM, P);
            for (I = 0; I < PK; I++)
            {
              for (J = 0; J < NumberLength; J++)
              {
                aiJW[I][J] = 0;
              }
            }
            for (I = 0; I < PL; I++)
            {
              for (J = 0; J < PL; J++)
              {
                MontgomeryMult(aiJS[I], aiJ01[J], biTmp);
                AddBigNbrModN(biTmp, aiJW[(I + J) % PK], aiJW[(I + J) % PK]);
              }
            }
            NormalizeJW(PK, PL, PM, P);
            MatchingRoot : do
            {
              H = -1;
              W = 0;
              for (I = 0; I < PL; I++)
              {
                if (BigNbrIsZero(aiJW[I]) == false)
                {
                  if (H == -1
                    && BigNbrAreEqual(aiJW[I], MontgomeryMultR1) == true)
                  {
                    H = I;
                  }
                  else
                  {
                    H = -2;
                    AddBigNbrModN(aiJW[I], MontgomeryMultR1, biTmp);
                    if (BigNbrIsZero(biTmp))
                    {
                      W++;
                    }
                  }
                }
              }
              if (H >= 0)
              {
                break MatchingRoot;
              }
              if (W != P - 1)
              {
                return 1; /* Not prime */
              }
              for (I = 0; I < PM; I++)
              {
                AddBigNbrModN(aiJW[I], MontgomeryMultR1, biTmp);
                if (BigNbrIsZero(biTmp) == true)
                {
                  break;
                }
              }
              if (I == PM)
              {
                return 1; /* Not prime */
              }
              for (J = 1; J <= P - 2; J++)
              {
                AddBigNbrModN(aiJW[I + J * PM], MontgomeryMultR1, biTmp);
                if (BigNbrIsZero(biTmp) == false)
                {
                  return 1; /* Not prime */
                }
              }
              H = I + PL;
            }
            while (false);
            if (SW == 1 || H % P == 0)
            {
              continue;
            }
            if (P != 2)
            {
              SW = 1;
              continue;
            }
            if (K == 1)
            {
              if ((TestNbr[0] & 3) == 1)
              {
                SW = 1;
              }
              continue;
            }

            // if (Q^((N-1)/2) mod N != N-1), N is not prime.

            MultBigNbrByLongModN(MontgomeryMultR1, Q, biTmp);
            for (I = 0; I < NumberLength; I++)
            {
              biR[I] = biTmp[I];
            }
            I = NumberLength - 1;
            Mask = 0x40000000l;
            while ((TestNbr[I] & Mask) == 0)
            {
              Mask /= 2;
              if (Mask == 0)
              {
                I--;
                Mask = 0x40000000l;
              }
            }
            do
            {
              Mask /= 2;
              if (Mask == 0)
              {
                I--;
                Mask = 0x40000000l;
              }
              MontgomeryMult(biR, biR, biT);
              for (J = 0; J < NumberLength; J++)
              {
                biR[J] = biT[J];
              }
              if ((TestNbr[I] & Mask) != 0)
              {
                MontgomeryMult(biR, biTmp, biT);
                for (J = 0; J < NumberLength; J++)
                {
                  biR[J] = biT[J];
                }
              }
            }
            while (I > 0 || Mask > 2);
            AddBigNbrModN(biR, MontgomeryMultR1, biTmp);
            if (BigNbrIsZero(biTmp) == false)
            {
              return 1; /* Not prime */
            }
            SW = 1;
          } /* end for j */
          if (SW == 0)
          {
            TestedQs = TestingQs + 1;
            if (TestingQs < aiNQ[LEVELnow] - 1)
            {
              TestingQs++;
              Q = aiQ[TestingQs];
              U = T * Q;
              do
              {
                MultBigNbrByLong(biS, Q, biS);
                U /= Q;
              }
              while (U % Q == 0);
              continue; /* Retry */
            }
            LEVELnow++;
            if (LEVELnow == LEVELmax)
            {
              return ProbabilisticPrimeTest(N); /* Cannot tell */
            }
            T = aiT[LEVELnow];
            NP = aiNP[LEVELnow];
            biS[0] = 2;
            for (J = 1; J < NumberLength; J++)
            {
              biS[J] = 0;
            }
            for (J = 0; J <= aiNQ[LEVELnow]; J++)
            {
              Q = aiQ[J];
              U = T * Q;
              do
              {
                MultBigNbrByLong(biS, Q, biS);
                U /= Q;
              }
              while (U % Q == 0);
              if (CompareSquare(biS, TestNbr) > 0)
              {
                TestingQs = J;
                continue MainStart; /* Retry from the beginning */
              }
            } /* end for J */
            return ProbabilisticPrimeTest(N); /* Program error */
          } /* end if */
          break;
        }
        while (true); /* end do */
      } /* end for i */

      // Final Test

      LengthN = NumberLength;
      for (I = 0; I < NumberLength; I++)
      {
        biN[I] = TestNbr[I];
        TestNbr[I] = biS[I];
        biR[I] = 0;
      }
      while (true)
      {
        if (TestNbr[NumberLength - 1] != 0)
        {
          break;
        }
        NumberLength--;
      }
      dN = (double) TestNbr[NumberLength - 1];
      if (NumberLength > 1)
      {
        dN += (double) TestNbr[NumberLength - 2] / dDosALa31;
      }
      if (NumberLength > 2)
      {
        dN += (double) TestNbr[NumberLength - 3] / dDosALa62;
      }
      LengthS = NumberLength;
      dS = dN;
      MontgomeryMultR1[0] = 1;
      for (I = 1; I < NumberLength; I++)
      {
        MontgomeryMultR1[I] = 0;
      }

      biR[0] = 1;
      BigNbrModN(biN, LengthN, biT); /* Compute N mod S */
      for (J = 1; J <= T; J++)
      {
        MultBigNbrModN(biR, biT, biTmp);
        for (i = NumberLength - 1; i > 0; i--)
        {
          if (biTmp[i] != 0)
          {
            break;
          }
        }
        if (i == 0 && biTmp[0] != 1)
        {
          return 0; /* Number is prime */
        }
        while (true)
        {
          if (biTmp[NumberLength - 1] != 0)
          {
            break;
          }
          NumberLength--;
        }
        for (I = 0; I < NumberLength; I++)
        {
          TestNbr[I] = biTmp[I];
        }
        dN = (double) TestNbr[NumberLength - 1];
        if (NumberLength > 1)
        {
          dN += (double) TestNbr[NumberLength - 2] / dDosALa31;
        }
        if (NumberLength > 2)
        {
          dN += (double) TestNbr[NumberLength - 3] / dDosALa62;
        }
        for (i = NumberLength - 1; i > 0; i--)
        {
          if (TestNbr[i] != biTmp[i])
          {
            break;
          }
        }
        if (TestNbr[i] > biTmp[i])
        {
          BigNbrModN(biN, LengthN, biTmp); /* Compute N mod R */
          if (BigNbrIsZero(biTmp) == true)
          { /* If N is multiple of R.. */
            return 1; /* Number is composite */
          }
        }
        dN = dS;
        NumberLength = LengthS;
        for (I = 0; I < NumberLength; I++)
        {
          biR[I] = TestNbr[I];
          TestNbr[I] = biS[I];
        }
      } /* End for J */
      return 0; /* Number is prime */
    }
    while (true);
  }

  // Prime checking routine
  // Return codes: 0 = Number is prime.
  //               1 = Number is composite.
  int ProbabilisticPrimeTest(BigInteger N)
  {
    long Base, Q;
    int baseNbr, nbrBases, exp, index, j, k;
    long mask;

    lowerTextArea.setText("Starting Rabin probabilistic prime check routine.");
    BigNbrToBigInt(N);
    exp = N.subtract(BigInt1).getLowestSetBit();
    GetMontgomeryParms();
    Base = 1;
    nbrBases = N.bitLength() / 2;
    for (baseNbr = 0; baseNbr < nbrBases; baseNbr++)
    {
      if (Base < 3)
      {
        Base++;
      }
      else
      {
        calculate_new_prime4 : do
        {
          Base += 2;
          for (Q = 3; Q * Q <= Base; Q += 2)
          { /* Check if Base is prime */
            if (Base % Q == 0)
            {
              continue calculate_new_prime4; /* Composite */
            }
          }
          break; /* Prime found */
        }
        while (true);
      } /* end if */
      lowerTextArea.setText(
        "Rabin probabilistic prime check routine\n\nBase used: "
          + Base
          + " ("
          + baseNbr * 100 / nbrBases
          + "%)");
      System.arraycopy(MontgomeryMultR1, 0, biN, 0, NumberLength);
      index = NumberLength - 1;
      mask = 0x40000000l;
      for (k = NumberLength * 31; k > exp; k--)
      {
        MontgomeryMult(biN, biN, biT);
        if ((TestNbr[index] & mask) != 0)
        {
          MultBigNbrByLongModN(biT, Base, biT);
        }
        System.arraycopy(biT, 0, biN, 0, NumberLength);
        mask /= 2;
        if (mask == 0)
        {
          index--;
          mask = 0x40000000l;
        }
      }
      for (j = 0; j < NumberLength; j++)
      {
        if (biN[j] != MontgomeryMultR1[j])
        {
          break;
        }
      }
      if (j == NumberLength)
      {
        continue;
      } /* Probable prime, go to next base */
      for (k = 0; k < exp; k++)
      {
        for (j = 0; j < NumberLength; j++)
        {
          if (biN[j] != MontgomeryMultR1[j])
          {
            break;
          }
        }
        if (j == NumberLength)
        {
          return 1;
        } /* Composite number */
        AddBigNbr(biN, MontgomeryMultR1, biT);
        for (j = 0; j < NumberLength; j++)
        {
          if (biT[j] != TestNbr[j])
          {
            break;
          }
        }
        if (j == NumberLength)
        {
          break;
        } /* Probable prime, go to next base */
        MontgomeryMult(biN, biN, biT);
        System.arraycopy(biT, 0, biN, 0, NumberLength);
      }
      if (k == exp)
      {
        return 1; /* Composite number */
      }
    }
    return 0; /* Indicate probable prime */
  }

  // Compare Nbr1^2 vs. Nbr2
  int CompareSquare(long Nbr1[], long Nbr2[])
  {
    int I, k;

    for (I = NumberLength - 1; I > 0; I--)
    {
      if (Nbr1[I] != 0)
      {
        break;
      }
    }
    k = NumberLength / 2;
    if (NumberLength % 2 == 0)
    {
      if (I >= k)
      {
        return 1;
      } // Nbr1^2 > Nbr2
      if (I < k - 1 || biS[k - 1] < 65536)
      {
        return -1;
      } // Nbr1^2 < Nbr2
    }
    else
    {
      if (I < k)
      {
        return -1;
      } // Nbr1^2 < Nbr2
      if (I > k || biS[k] >= 65536)
      {
        return 1;
      } // Nbr1^2 > Nbr2
    }
    MultBigNbr(biS, biS, biTmp);
    SubtractBigNbr(biTmp, TestNbr, biTmp);
    if (BigNbrIsZero(biTmp) == true)
    {
      return 0;
    } // Nbr1^2 == Nbr2
    if (biTmp[NumberLength - 1] >= 0)
    {
      return 1;
    } // Nbr1^2 > Nbr2
    return -1; // Nbr1^2 < Nbr2
  }

  // Perform JS <- JS ^ E

  void JS_E(int PK, int PL, int PM, int P)
  {
    int I, J, K;
    long Mask;

    for (I = NumberLength - 1; I > 0; I--)
    {
      if (biExp[I] != 0)
      {
        break;
      }
    }
    if (I == 0 && biExp[0] == 1)
    {
      return;
    } // Return if E == 1
    for (K = 0; K < PL; K++)
    {
      for (J = 0; J < NumberLength; J++)
      {
        aiJW[K][J] = aiJS[K][J];
      }
    }
    Mask = 0x40000000l;
    while (true)
    {
      if ((biExp[I] & Mask) != 0)
      {
        break;
      }
      Mask /= 2;
    }
    do
    {
      JS_2(PK, PL, PM, P);
      Mask /= 2;
      if (Mask == 0)
      {
        Mask = 0x40000000l;
        I--;
      }
      if ((biExp[I] & Mask) != 0)
      {
        JS_JW(PK, PL, PM, P);
      }
    }
    while (I > 0 || Mask != 1);
  }

  // Perform JS <- JS * JW

  void JS_JW(int PK, int PL, int PM, int P)
  {
    int I, J, K;
    for (I = 0; I < PL; I++)
    {
      for (J = 0; J < PL; J++)
      {
        K = (I + J) % PK;
        MontgomeryMult(aiJS[I], aiJW[J], biTmp);
        AddBigNbrModN(aiJX[K], biTmp, aiJX[K]);
      }
    }
    for (I = 0; I < PK; I++)
    {
      for (J = 0; J < NumberLength; J++)
      {
        aiJS[I][J] = aiJX[I][J];
        aiJX[I][J] = 0;
      }
    }
    NormalizeJS(PK, PL, PM, P);
  }

  // Perform JS <- JS ^ 2

  void JS_2(int PK, int PL, int PM, int P)
  {
    int I, J, K;
    for (I = 0; I < PL; I++)
    {
      K = 2 * I % PK;
      MontgomeryMult(aiJS[I], aiJS[I], biTmp);
      AddBigNbrModN(aiJX[K], biTmp, aiJX[K]);
      AddBigNbrModN(aiJS[I], aiJS[I], biT);
      for (J = I + 1; J < PL; J++)
      {
        K = (I + J) % PK;
        MontgomeryMult(biT, aiJS[J], biTmp);
        AddBigNbrModN(aiJX[K], biTmp, aiJX[K]);
      }
    }
    for (I = 0; I < PK; I++)
    {
      for (J = 0; J < NumberLength; J++)
      {
        aiJS[I][J] = aiJX[I][J];
        aiJX[I][J] = 0;
      }
    }
    NormalizeJS(PK, PL, PM, P);
  }

  // Normalize coefficient of JS
  void NormalizeJS(int PK, int PL, int PM, int P)
  {
    int I, J;
    for (I = PL; I < PK; I++)
    {
      if (BigNbrIsZero(aiJS[I]) == false)
      {
        for (J = 0; J < NumberLength; J++)
        {
          biT[J] = aiJS[I][J];
        }
        for (J = 1; J < P; J++)
        {
          SubtractBigNbrModN(aiJS[I - J * PM], biT, aiJS[I - J * PM]);
        }
        for (J = 0; J < NumberLength; J++)
        {
          aiJS[I][J] = 0;
        }
      }
    }
  }

  // Normalize coefficient of JW
  void NormalizeJW(int PK, int PL, int PM, int P)
  {
    int I, J;
    for (I = PL; I < PK; I++)
    {
      if (BigNbrIsZero(aiJW[I]) == false)
      {
        for (J = 0; J < NumberLength; J++)
        {
          biT[J] = aiJW[I][J];
        }
        for (J = 1; J < P; J++)
        {
          SubtractBigNbrModN(aiJW[I - J * PM], biT, aiJW[I - J * PM]);
        }
        for (J = 0; J < NumberLength; J++)
        {
          aiJW[I][J] = 0;
        }
      }
    }
  }

  void JacobiSum(int A, int B, int P, int PK, int PL, int PM, int Q)
  {
    int I, J, K;

    for (I = 0; I < PL; I++)
    {
      for (J = 0; J < NumberLength; J++)
      {
        aiJ0[I][J] = 0;
      }
    }
    for (I = 1; I <= Q - 2; I++)
    {
      J = (A * I + B * aiF[I]) % PK;
      if (J < PL)
      {
        AddBigNbrModN(aiJ0[J], MontgomeryMultR1, aiJ0[J]);
      }
      else
      {
        for (K = 1; K < P; K++)
        {
          SubtractBigNbrModN(
            aiJ0[J - K * PM],
            MontgomeryMultR1,
            aiJ0[J - K * PM]);
        }
      }
    }
  }

  BigInteger fnECM(BigInteger N, int FactorIndex)
  {
    int I, J, Pass, Qaux;
    long L1, L2, LS, P, Q, K, IP, Paux = 1;
    long[] A0 = new long[NLen];
    long[] A02 = new long[NLen];
    long[] A03 = new long[NLen];
    long[] AA = new long[NLen];
    long[] DX = new long[NLen];
    long[] DZ = new long[NLen];
    long[] GD = new long[NLen];
    long[] M = new long[NLen];
    long[] N1 = new long[NLen];
    long[] N2 = new long[NLen];
    long[] TX = new long[NLen];
    fieldTX = TX;
    long[] TZ = new long[NLen];
    fieldTZ = TZ;
    long[] UX = new long[NLen];
    fieldUX = UX;
    long[] UZ = new long[NLen];
    fieldUZ = UZ;
    long[] W0 = new long[NLen];
    long[] W1 = new long[NLen];
    long[] W2 = new long[NLen];
    long[] W3 = new long[NLen];
    long[] W4 = new long[NLen];
    long[] WX = new long[NLen];
    long[] WZ = new long[NLen];
    long[] X = new long[NLen];
    long[] Z = new long[NLen];
    long[] Aux1 = new long[NLen];
    fieldAux1 = Aux1;
    long[] Aux2 = new long[NLen];
    fieldAux2 = Aux2;
    long[] Aux3 = new long[NLen];
    fieldAux3 = Aux3;
    long[] Aux4 = new long[NLen];
    fieldAux4 = Aux4;
    long[] Xaux = new long[NLen];
    long[] Zaux = new long[NLen];
    long[][] root = new long[480][NLen];
    byte[] Result;
    byte[] sieve = new byte[23100];
    byte[] sieve2310 = new byte[2310];
    int[] sieveidx = new int[480];
    String UpperLine;
    String LowerLine;
    String primalityString;
    int Prob, i, j, u;
    BigInteger NN;
    boolean PrevCurveECMd = false;

    fieldAA = AA;
    BigNbrToBigInt(N);
    GetMontgomeryParms();
    for (I = 0; I < NumberLength; I++)
    {
      M[I] = DX[I] = DZ[I] = W3[I] = W4[I] = GD[I] = 0;
    }
    textAreaContents = "";
    StringToLabel = "Factoring ";
    insertBigNbr(N);
    addStringToLabel("(" + N.toString().length() + " digits)");
    EC--;
    SmallPrime[0] = 2;
    P = 3;
    indexM = 1;
    for (indexM = 1; indexM < SmallPrime.length; indexM++)
    {
      SmallPrime[indexM] = (int) P; /* Store prime */
      calculate_new_prime1 : do
      {
        P += 2;
        for (Q = 3; Q * Q <= P; Q += 2)
        { /* Check if P is prime */
          if (P % Q == 0)
          {
            continue calculate_new_prime1; /* Composite */
          }
        }
        break; /* Prime found */
      }
      while (true);
    }
    foundByLehman = false;
    do
    {
      new_curve : do
      {
        if (NextEC > 0)
        {
          EC = NextEC;
          NextEC = -1;
          if (EC >= TYP_SIQS)
          {
            return BigInt1;
          }
        }
        else
        {
          EC++;
          NN = Lehman(NumberToFactor, EC);
          if (NN.equals(BigInt1) == false)
          {                // Factor found.
            foundByLehman = true; 
            return NN;
          }
          L1 = N.toString().length();   // Get number of digits.
          if (L1 > 30 && L1 <= 90)      // If between 30 and 90 digits...
          {
            if ((digitsInGroup & 0x400) == 0)
            {                           // Switch to SIQS checkbox is set.
              int limit = limits[((int)L1 - 31) / 5];
              if (EC % 50000000 > limit && PrevCurveECMd == false ||
                  EC % 50000000 == limit && PrevCurveECMd == true)
              {                         // Switch to SIQS.
                EC += TYP_SIQS;
                return BigInt1;
              }
            }
          }
        }
        PrevCurveECMd = true;
        Typ[FactorIndex] = EC;
        L1 = 2000;
        L2 = 200000;
        LS = 45;
        Paux = EC;
        NbrPrimes = 303; /* Number of primes less than 2000 */
        if (EC > 25)
        {
          if (EC < 326)
          {
            L1 = 50000;
            L2 = 5000000;
            LS = 224;
            Paux = EC - 24;
            NbrPrimes = 5133; /* Number of primes less than 50000 */
          }
          else
          {
            if (EC < 2000)
            {
              L1 = 1000000;
              L2 = 100000000;
              LS = 1001;
              Paux = EC - 299;
              NbrPrimes = 78498; /* Number of primes less than 1000000 */
            }
            else
            {
              L1 = 11000000;
              L2 = 1100000000;
              LS = 3316;
              Paux = EC - 1900;
              NbrPrimes = 726517; /* Number of primes less than 11000000 */
            }
          }
        }
        primalityString =
          textAreaContents
            + StringToLabel
            + "\nLimit (B1="
            + L1
            + "; B2="
            + L2
            + ")    Curve ";
        UpperLine = "Digits in factor:   ";
        LowerLine = "Probability:        ";
        for (I = 0; I < 6; I++)
        {
          UpperLine += "    >= " + (I * 5 + 15);
          Prob =
            (int) Math.round(
              100
                * (1 - Math.exp(- ((double) L1 * (double) Paux) / ProbArray[I])));
          if (Prob == 100)
          {
            LowerLine += "    100% ";
          }
          else
          {
            if (Prob >= 10)
            {
              LowerLine += "     " + Prob + "% ";
            }
            else
            {
              LowerLine += "      " + Prob + "% ";
            }
          }
        } /* end for */
        lowerTextArea.setText(
          primalityString + EC + "\n" + UpperLine + "\n" + LowerLine);
        LongToBigNbr(2 * (EC + 1), Aux1);
        LongToBigNbr(3 * (EC + 1) * (EC + 1) - 1, Aux2);
        ModInvBigNbr(Aux2, Aux2, TestNbr);
        MultBigNbrModN(Aux1, Aux2, Aux3);
        MultBigNbrModN(Aux3, MontgomeryMultR1, A0);
        MontgomeryMult(A0, A0, A02);
        MontgomeryMult(A02, A0, A03);
        SubtractBigNbrModN(A03, A0, Aux1);
        MultBigNbrByLongModN(A02, 9, Aux2);
        SubtractBigNbrModN(Aux2, MontgomeryMultR1, Aux2);
        MontgomeryMult(Aux1, Aux2, Aux3);
        if (BigNbrIsZero(Aux3))
        {
          continue;
        }
        MultBigNbrByLongModN(A0, 4, Z);
        MultBigNbrByLongModN(A02, 6, Aux1);
        SubtractBigNbrModN(MontgomeryMultR1, Aux1, Aux1);
        MontgomeryMult(A02, A02, Aux2);
        MultBigNbrByLongModN(Aux2, 3, Aux2);
        SubtractBigNbrModN(Aux1, Aux2, Aux1);
        MultBigNbrByLongModN(A03, 4, Aux2);
        ModInvBigNbr(Aux2, Aux2, TestNbr);
        MontgomeryMult(Aux2, MontgomeryMultAfterInv, Aux3);
        MontgomeryMult(Aux1, Aux3, A0);
        AddBigNbrModN(A0, MontgomeryMultR2, Aux1);
        LongToBigNbr(4, Aux2);
        ModInvBigNbr(Aux2, Aux3, TestNbr);
        MultBigNbrModN(Aux3, MontgomeryMultR1, Aux2);
        MontgomeryMult(Aux1, Aux2, AA);
        MultBigNbrByLongModN(A02, 3, Aux1);
        AddBigNbrModN(Aux1, MontgomeryMultR1, X);
        /**************/
        /* First step */
        /**************/
        System.arraycopy(X, 0, Xaux, 0, NumberLength);
        System.arraycopy(Z, 0, Zaux, 0, NumberLength);
        System.arraycopy(MontgomeryMultR1, 0, GcdAccumulated, 0, NumberLength);
        for (Pass = 0; Pass < 2; Pass++)
        {
          /* For powers of 2 */
          indexPrimes = 0;
          StepECM = 1;
          for (I = 1; I <= L1; I <<= 1)
          {
            duplicate(X, Z, X, Z);
          }
          for (I = 3; I <= L1; I *= 3)
          {
            duplicate(W1, W2, X, Z);
            add3(X, Z, X, Z, W1, W2, X, Z);
          }

          if (Pass == 0)
          {
            MontgomeryMult(GcdAccumulated, Z, Aux1);
            System.arraycopy(Aux1, 0, GcdAccumulated, 0, NumberLength);
          }
          else
          {
            GcdBigNbr(Z, TestNbr, GD);
            if (BigNbrAreEqual(GD, BigNbr1) == false)
            {
              break new_curve;
            }
          }

          /* for powers of odd primes */

          indexM = 1;
          do
          {
            indexPrimes++;
            P = SmallPrime[indexM];
            for (IP = P; IP <= L1; IP *= P)
            {
              prac((int) P, X, Z, W1, W2, W3, W4);
            }
            indexM++;
            if (Pass == 0)
            {
              MontgomeryMult(GcdAccumulated, Z, Aux1);
              System.arraycopy(Aux1, 0, GcdAccumulated, 0, NumberLength);
            }
            else
            {
              GcdBigNbr(Z, TestNbr, GD);
              if (BigNbrAreEqual(GD, BigNbr1) == false)
              {
                break new_curve;
              }
            }
          }
          while (SmallPrime[indexM - 1] <= LS);
          P += 2;

          /* Initialize sieve2310[n]: 1 if gcd(P+2n,2310) > 1, 0 otherwise */
          u = (int) P;
          for (i = 0; i < 2310; i++)
          {
            sieve2310[i] =
              (u % 3 == 0
                || u % 5 == 0
                || u % 7 == 0
                || u % 11 == 0 ? (byte) 1 : (byte) 0);
            u += 2;
          }
          do
          {
            /* Generate sieve */
            GenerateSieve((int) P, sieve, sieve2310, SmallPrime);

            /* Walk through sieve */

            for (i = 0; i < 23100; i++)
            {
              if (sieve[i] != 0)
                continue; /* Do not process composites */
              if (P + 2 * i > L1)
                break;
              indexPrimes++;
              prac((int) (P + 2 * i), X, Z, W1, W2, W3, W4);
              if (Pass == 0)
              {
                MontgomeryMult(GcdAccumulated, Z, Aux1);
                System.arraycopy(Aux1, 0, GcdAccumulated, 0, NumberLength);
              }
              else
              {
                GcdBigNbr(Z, TestNbr, GD);
                if (BigNbrAreEqual(GD, BigNbr1) == false)
                {
                  break new_curve;
                }
              }
            }
            P += 46200;
          }
          while (P < L1);
          if (Pass == 0)
          {
            if (BigNbrIsZero(GcdAccumulated))
            { // If GcdAccumulated is
              System.arraycopy(Xaux, 0, X, 0, NumberLength);
              System.arraycopy(Zaux, 0, Z, 0, NumberLength);
              continue; // multiple of TestNbr, continue.
            }
            GcdBigNbr(GcdAccumulated, TestNbr, GD);
            if (BigNbrAreEqual(GD, BigNbr1) == false)
            {
              break new_curve;
            }
            break;
          }
        } /* end for Pass */

        /******************************************************/
        /* Second step (using improved standard continuation) */
        /******************************************************/
        StepECM = 2;
        j = 0;
        for (u = 1; u < 2310; u += 2)
        {
          if (u % 3 == 0 || u % 5 == 0 || u % 7 == 0 || u % 11 == 0)
          {
            sieve2310[u / 2] = (byte) 1;
          }
          else
          {
            sieve2310[(sieveidx[j++] = u / 2)] = (byte) 0;
          }
        }
        System.arraycopy(sieve2310, 0, sieve2310, 1155, 1155);
        System.arraycopy(X, 0, Xaux, 0, NumberLength); // (X:Z) -> Q (output
        System.arraycopy(Z, 0, Zaux, 0, NumberLength); //         from step 1)
        for (Pass = 0; Pass < 2; Pass++)
        {
          System.arraycopy(
            MontgomeryMultR1,
            0,
            GcdAccumulated,
            0,
            NumberLength);
          System.arraycopy(X, 0, UX, 0, NumberLength);
          System.arraycopy(Z, 0, UZ, 0, NumberLength); // (UX:UZ) -> Q
          ModInvBigNbr(Z, Aux2, TestNbr);
          MontgomeryMult(Aux2, MontgomeryMultAfterInv, Aux1);
          MontgomeryMult(Aux1, X, root[0]); // root[0] <- X/Z (Q)
          J = 0;
          AddBigNbrModN(X, Z, Aux1);
          MontgomeryMult(Aux1, Aux1, W1);
          SubtractBigNbrModN(X, Z, Aux1);
          MontgomeryMult(Aux1, Aux1, W2);
          MontgomeryMult(W1, W2, TX);
          SubtractBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, AA, Aux2);
          AddBigNbrModN(Aux2, W2, Aux3);
          MontgomeryMult(Aux1, Aux3, TZ); // (TX:TZ) -> 2Q
          SubtractBigNbrModN(X, Z, Aux1);
          AddBigNbrModN(TX, TZ, Aux2);
          MontgomeryMult(Aux1, Aux2, W1);
          AddBigNbrModN(X, Z, Aux1);
          SubtractBigNbrModN(TX, TZ, Aux2);
          MontgomeryMult(Aux1, Aux2, W2);
          AddBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, Aux1, Aux2);
          MontgomeryMult(Aux2, UZ, X);
          SubtractBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, Aux1, Aux2);
          MontgomeryMult(Aux2, UX, Z); // (X:Z) -> 3Q
          for (I = 5; I < 2310; I += 2)
          {
            System.arraycopy(X, 0, WX, 0, NumberLength);
            System.arraycopy(Z, 0, WZ, 0, NumberLength);
            SubtractBigNbrModN(X, Z, Aux1);
            AddBigNbrModN(TX, TZ, Aux2);
            MontgomeryMult(Aux1, Aux2, W1);
            AddBigNbrModN(X, Z, Aux1);
            SubtractBigNbrModN(TX, TZ, Aux2);
            MontgomeryMult(Aux1, Aux2, W2);
            AddBigNbrModN(W1, W2, Aux1);
            MontgomeryMult(Aux1, Aux1, Aux2);
            MontgomeryMult(Aux2, UZ, X);
            SubtractBigNbrModN(W1, W2, Aux1);
            MontgomeryMult(Aux1, Aux1, Aux2);
            MontgomeryMult(Aux2, UX, Z); // (X:Z) -> 5Q, 7Q, ...
            if (Pass == 0)
            {
              MontgomeryMult(GcdAccumulated, Aux1, Aux2);
              System.arraycopy(Aux2, 0, GcdAccumulated, 0, NumberLength);
            }
            else
            {
              GcdBigNbr(Aux1, TestNbr, GD);
              if (BigNbrAreEqual(GD, BigNbr1) == false)
              {
                break new_curve;
              }
            }
            if (I == 1155)
            {
              System.arraycopy(X, 0, DX, 0, NumberLength);
              System.arraycopy(Z, 0, DZ, 0, NumberLength); // (DX:DZ) -> 1155Q
            }
            if (I % 3 != 0 && I % 5 != 0 && I % 7 != 0 && I % 11 != 0)
            {
              J++;
              ModInvBigNbr(Z, Aux2, TestNbr);
              MontgomeryMult(Aux2, MontgomeryMultAfterInv, Aux1);
              MontgomeryMult(Aux1, X, root[J]); // root[J] <- X/Z
            }
            System.arraycopy(WX, 0, UX, 0, NumberLength); // (UX:UZ) <-
            System.arraycopy(WZ, 0, UZ, 0, NumberLength); // Previous (X:Z)
          } /* end for I */
          AddBigNbrModN(DX, DZ, Aux1);
          MontgomeryMult(Aux1, Aux1, W1);
          SubtractBigNbrModN(DX, DZ, Aux1);
          MontgomeryMult(Aux1, Aux1, W2);
          MontgomeryMult(W1, W2, X);
          SubtractBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, AA, Aux2);
          AddBigNbrModN(Aux2, W2, Aux3);
          MontgomeryMult(Aux1, Aux3, Z);
          System.arraycopy(X, 0, UX, 0, NumberLength);
          System.arraycopy(Z, 0, UZ, 0, NumberLength); // (UX:UZ) -> 2310Q
          AddBigNbrModN(X, Z, Aux1);
          MontgomeryMult(Aux1, Aux1, W1);
          SubtractBigNbrModN(X, Z, Aux1);
          MontgomeryMult(Aux1, Aux1, W2);
          MontgomeryMult(W1, W2, TX);
          SubtractBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, AA, Aux2);
          AddBigNbrModN(Aux2, W2, Aux3);
          MontgomeryMult(Aux1, Aux3, TZ); // (TX:TZ) -> 2*2310Q
          SubtractBigNbrModN(X, Z, Aux1);
          AddBigNbrModN(TX, TZ, Aux2);
          MontgomeryMult(Aux1, Aux2, W1);
          AddBigNbrModN(X, Z, Aux1);
          SubtractBigNbrModN(TX, TZ, Aux2);
          MontgomeryMult(Aux1, Aux2, W2);
          AddBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, Aux1, Aux2);
          MontgomeryMult(Aux2, UZ, X);
          SubtractBigNbrModN(W1, W2, Aux1);
          MontgomeryMult(Aux1, Aux1, Aux2);
          MontgomeryMult(Aux2, UX, Z); // (X:Z) -> 3*2310Q
          Qaux = (int) (L1 / 4620);
          maxIndexM = (int) (L2 / 4620);
          for (indexM = 0; indexM <= maxIndexM; indexM++)
          {
            if (indexM >= Qaux)
            { // If inside step 2 range... 
              if (indexM == 0)
              {
                ModInvBigNbr(UZ, Aux2, TestNbr);
                MontgomeryMult(Aux2, MontgomeryMultAfterInv, Aux3);
                MontgomeryMult(UX, Aux3, Aux1); // Aux1 <- X/Z (2310Q)
              }
              else
              {
                ModInvBigNbr(Z, Aux2, TestNbr);
                MontgomeryMult(Aux2, MontgomeryMultAfterInv, Aux3);
                MontgomeryMult(X, Aux3, Aux1); // Aux1 <- X/Z (3,5,*
              } //              2310Q)

              /* Generate sieve */
              if (indexM % 10 == 0 || indexM == Qaux)
              {
                GenerateSieve(
                  indexM / 10 * 46200 + 1,
                  sieve,
                  sieve2310,
                  SmallPrime);
              }
              /* Walk through sieve */
              J = 1155 + (indexM % 10) * 2310;
              for (i = 0; i < 480; i++)
              {
                j = sieveidx[i]; // 0 < J < 1155
                if (sieve[J + j] != 0 && sieve[J - 1 - j] != 0)
                {
                  continue; // Do not process if both are composite numbers.
                }
                SubtractBigNbrModN(Aux1, root[i], M);
                MontgomeryMult(GcdAccumulated, M, Aux2);
                System.arraycopy(Aux2, 0, GcdAccumulated, 0, NumberLength);
              }
              if (Pass != 0)
              {
                GcdBigNbr(GcdAccumulated, TestNbr, GD);
                if (BigNbrAreEqual(GD, BigNbr1) == false)
                {
                  break new_curve;
                }
              }
            }
            if (indexM != 0)
            { // Update (X:Z)
              System.arraycopy(X, 0, WX, 0, NumberLength);
              System.arraycopy(Z, 0, WZ, 0, NumberLength);
              SubtractBigNbrModN(X, Z, Aux1);
              AddBigNbrModN(TX, TZ, Aux2);
              MontgomeryMult(Aux1, Aux2, W1);
              AddBigNbrModN(X, Z, Aux1);
              SubtractBigNbrModN(TX, TZ, Aux2);
              MontgomeryMult(Aux1, Aux2, W2);
              AddBigNbrModN(W1, W2, Aux1);
              MontgomeryMult(Aux1, Aux1, Aux2);
              MontgomeryMult(Aux2, UZ, X);
              SubtractBigNbrModN(W1, W2, Aux1);
              MontgomeryMult(Aux1, Aux1, Aux2);
              MontgomeryMult(Aux2, UX, Z);
              System.arraycopy(WX, 0, UX, 0, NumberLength);
              System.arraycopy(WZ, 0, UZ, 0, NumberLength);
            }
          } // end for Q
          if (Pass == 0)
          {
            if (BigNbrIsZero(GcdAccumulated))
            { // If GcdAccumulated is
              System.arraycopy(Xaux, 0, X, 0, NumberLength);
              System.arraycopy(Zaux, 0, Z, 0, NumberLength);
              continue; // multiple of TestNbr, continue.
            }
            GcdBigNbr(GcdAccumulated, TestNbr, GD);
            if (BigNbrAreEqual(GD, TestNbr) == true)
            {
              break;
            }
            if (BigNbrAreEqual(GD, BigNbr1) == false)
            {
              break new_curve;
            }
            break;
          }
        } /* end for Pass */
        performLehman = true;
      }
      while (true); /* end curve calculation */
    }
    while (BigNbrAreEqual(GD, TestNbr) == true);
    lowerTextArea.setText("");
    StepECM = 0; /* do not show pass number on screen */
    return BigIntToBigNbr(GD);
  }

  /**********************/
  /* Auxiliary routines */
  /**********************/

  static void GenerateSieve(
    int initial,
    byte[] sieve,
    byte[] sieve2310,
    int[] SmallPrime)
  {
    int i, j, Q;
    for (i = 0; i < 23100; i += 2310)
    {
      System.arraycopy(sieve2310, 0, sieve, i, 2310);
    }
    j = 5;
    Q = 13; /* Point to prime 13 */
    do
    {
      if (initial > Q * Q)
      {
        for (i = (int) (((long) initial * ((Q - 1) / 2)) % Q);
          i < 23100;
          i += Q)
        {
          sieve[i] = 1; /* Composite */
        }
      }
      else
      {
        i = Q * Q - initial;
        if (i < 46200)
        {
          for (i = i / 2; i < 23100; i += Q)
          {
            sieve[i] = 1; /* Composite */
          }
        }
        else
        {
          break;
        }
      }
      Q = SmallPrime[++j];
    }
    while (Q < 5000);
  }

  void BigNbrToBigInt(BigInteger N)
  {
    byte[] Result;
    long[] Temp;
    int i, j, k;
    long mask, p;

    Result = N.toByteArray();
    NumberLength = (Result.length * 8 + 30)/31;
    Temp = new long[NumberLength+1];
    yieldFreq = 1000000 / (NumberLength * NumberLength);
    if (yieldFreq > 100000)
      yieldFreq = yieldFreq / 100000 * 100000;
    else if (yieldFreq > 10000)
      yieldFreq = yieldFreq / 10000 * 10000;
    else if (yieldFreq > 1000)
      yieldFreq = yieldFreq / 1000 * 1000;
    else if (yieldFreq > 100)
      yieldFreq = yieldFreq / 100 * 100;
    j = 0;
    mask = 1;
    p = 0;
    for (i = Result.length - 1; i >= 0; i--)
    {
      p += mask * (long) (Result[i] >= 0 ? Result[i] : Result[i] + 256);
      mask *= 0x100;
      if (mask == DosALa32)
      {
        Temp[j++] = p;
        mask = 1;
        p = 0;
      }
    }
    Temp[j] = p;
    Convert32To31Bits(Temp, TestNbr);
    if (TestNbr[NumberLength - 1] > Mi)
    {
      TestNbr[NumberLength] = 0;
      NumberLength++;
    }
    TestNbr[NumberLength] = 0;
  }

  /*********************************************************/
  /* Start of code "borrowed" from Paul Zimmermann's ECM4C */
  /*********************************************************/
  final static int ADD = 6; /* number of multiplications in an addition */
  final static int DUP = 5; /* number of multiplications in a duplicate */

  /* returns the number of modular multiplications */
  static int lucas_cost(int n, double v)
  {
    int c, d, e, r;

    d = n;
    r = (int) ((double) d / v + 0.5);
    if (r >= n)
      return (ADD * n);
    d = n - r;
    e = 2 * r - n;
    c = DUP + ADD; /* initial duplicate and final addition */
    while (d != e)
    {
      if (d < e)
      {
        r = d;
        d = e;
        e = r;
      }
      if (4 * d <= 5 * e && ((d + e) % 3) == 0)
      { /* condition 1 */
        r = (2 * d - e) / 3;
        e = (2 * e - d) / 3;
        d = r;
        c += 3 * ADD; /* 3 additions */
      }
      else if (4 * d <= 5 * e && (d - e) % 6 == 0)
      { /* condition 2 */
        d = (d - e) / 2;
        c += ADD + DUP; /* one addition, one duplicate */
      }
      else if (d <= (4 * e))
      { /* condition 3 */
        d -= e;
        c += ADD; /* one addition */
      }
      else if ((d + e) % 2 == 0)
      { /* condition 4 */
        d = (d - e) / 2;
        c += ADD + DUP; /* one addition, one duplicate */
      }
      else if (d % 2 == 0)
      { /* condition 5 */
        d /= 2;
        c += ADD + DUP; /* one addition, one duplicate */
      }
      else if (d % 3 == 0)
      { /* condition 6 */
        d = d / 3 - e;
        c += 3 * ADD + DUP; /* three additions, one duplicate */
      }
      else if ((d + e) % 3 == 0)
      { /* condition 7 */
        d = (d - 2 * e) / 3;
        c += 3 * ADD + DUP; /* three additions, one duplicate */
      }
      else if ((d - e) % 3 == 0)
      { /* condition 8 */
        d = (d - e) / 3;
        c += 3 * ADD + DUP; /* three additions, one duplicate */
      }
      else if (e % 2 == 0)
      { /* condition 9 */
        e /= 2;
        c += ADD + DUP; /* one addition, one duplicate */
      }
    }
    return (c);
  }

  /* computes nP from P=(x:z) and puts the result in (x:z). Assumes n>2. */
  void prac(
    int n,
    long[] x,
    long[] z,
    long[] xT,
    long[] zT,
    long[] xT2,
    long[] zT2)
  {
    int d, e, r, i;
    long[] t;
    long[] xA = x, zA = z;
    long[] xB = fieldAux1, zB = fieldAux2;
    long[] xC = fieldAux3, zC = fieldAux4;
    double v[] =
      {
        1.61803398875,
        1.72360679775,
        1.618347119656,
        1.617914406529,
        1.612429949509,
        1.632839806089,
        1.620181980807,
        1.580178728295,
        1.617214616534,
        1.38196601125 };

    /* chooses the best value of v */
    r = lucas_cost(n, v[0]);
    i = 0;
    for (d = 1; d < 10; d++)
    {
      e = lucas_cost(n, v[d]);
      if (e < r)
      {
        r = e;
        i = d;
      }
    }
    d = n;
    r = (int) ((double) d / v[i] + 0.5);
    /* first iteration always begins by Condition 3, then a swap */
    d = n - r;
    e = 2 * r - n;
    System.arraycopy(xA, 0, xB, 0, NumberLength); // B = A
    System.arraycopy(zA, 0, zB, 0, NumberLength);
    System.arraycopy(xA, 0, xC, 0, NumberLength); // C = A
    System.arraycopy(zA, 0, zC, 0, NumberLength);
    duplicate(xA, zA, xA, zA); /* A=2*A */
    while (d != e)
    {
      if (d < e)
      {
        r = d;
        d = e;
        e = r;
        t = xA;
        xA = xB;
        xB = t;
        t = zA;
        zA = zB;
        zB = t;
      }
      /* do the first line of Table 4 whose condition qualifies */
      if (4 * d <= 5 * e && ((d + e) % 3) == 0)
      { /* condition 1 */
        r = (2 * d - e) / 3;
        e = (2 * e - d) / 3;
        d = r;
        add3(xT, zT, xA, zA, xB, zB, xC, zC); /* T = f(A,B,C) */
        add3(xT2, zT2, xT, zT, xA, zA, xB, zB); /* T2 = f(T,A,B) */
        add3(xB, zB, xB, zB, xT, zT, xA, zA); /* B = f(B,T,A) */
        t = xA;
        xA = xT2;
        xT2 = t;
        t = zA;
        zA = zT2;
        zT2 = t; /* swap A and T2 */
      }
      else if (4 * d <= 5 * e && (d - e) % 6 == 0)
      { /* condition 2 */
        d = (d - e) / 2;
        add3(xB, zB, xA, zA, xB, zB, xC, zC); /* B = f(A,B,C) */
        duplicate(xA, zA, xA, zA); /* A = 2*A */
      }
      else if (d <= (4 * e))
      { /* condition 3 */
        d -= e;
        add3(xT, zT, xB, zB, xA, zA, xC, zC); /* T = f(B,A,C) */
        t = xB;
        xB = xT;
        xT = xC;
        xC = t;
        t = zB;
        zB = zT;
        zT = zC;
        zC = t; /* circular permutation (B,T,C) */
      }
      else if ((d + e) % 2 == 0)
      { /* condition 4 */
        d = (d - e) / 2;
        add3(xB, zB, xB, zB, xA, zA, xC, zC); /* B = f(B,A,C) */
        duplicate(xA, zA, xA, zA); /* A = 2*A */
      }
      else if (d % 2 == 0)
      { /* condition 5 */
        d /= 2;
        add3(xC, zC, xC, zC, xA, zA, xB, zB); /* C = f(C,A,B) */
        duplicate(xA, zA, xA, zA); /* A = 2*A */
      }
      else if (d % 3 == 0)
      { /* condition 6 */
        d = d / 3 - e;
        duplicate(xT, zT, xA, zA); /* T1 = 2*A */
        add3(xT2, zT2, xA, zA, xB, zB, xC, zC); /* T2 = f(A,B,C) */
        add3(xA, zA, xT, zT, xA, zA, xA, zA); /* A = f(T1,A,A) */
        add3(xT, zT, xT, zT, xT2, zT2, xC, zC); /* T1 = f(T1,T2,C) */
        t = xC;
        xC = xB;
        xB = xT;
        xT = t;
        t = zC;
        zC = zB;
        zB = zT;
        zT = t; /* circular permutation (C,B,T) */
      }
      else if ((d + e) % 3 == 0)
      { /* condition 7 */
        d = (d - 2 * e) / 3;
        add3(xT, zT, xA, zA, xB, zB, xC, zC); /* T1 = f(A,B,C) */
        add3(xB, zB, xT, zT, xA, zA, xB, zB); /* B = f(T1,A,B) */
        duplicate(xT, zT, xA, zA);
        add3(xA, zA, xA, zA, xT, zT, xA, zA); /* A = 3*A */
      }
      else if ((d - e) % 3 == 0)
      { /* condition 8 */
        d = (d - e) / 3;
        add3(xT, zT, xA, zA, xB, zB, xC, zC); /* T1 = f(A,B,C) */
        add3(xC, zC, xC, zC, xA, zA, xB, zB); /* C = f(A,C,B) */
        t = xB;
        xB = xT;
        xT = t;
        t = zB;
        zB = zT;
        zT = t; /* swap B and T */
        duplicate(xT, zT, xA, zA);
        add3(xA, zA, xA, zA, xT, zT, xA, zA); /* A = 3*A */
      }
      else if (e % 2 == 0)
      { /* condition 9 */
        e /= 2;
        add3(xC, zC, xC, zC, xB, zB, xA, zA); /* C = f(C,B,A) */
        duplicate(xB, zB, xB, zB); /* B = 2*B */
      }
    }
    add3(x, z, xA, zA, xB, zB, xC, zC);
  }

  /* adds Q=(x2:z2) and R=(x1:z1) and puts the result in (x3:z3),
       using 5/6 mul, 6 add/sub and 6 mod. One assumes that Q-R=P or R-Q=P where P=(x:z).
       Uses the following global variables:
       - n : number to factor
       - x, z : coordinates of P
       - u, v, w : auxiliary variables
  Modifies: x3, z3, u, v, w.
  (x3,z3) may be identical to (x2,z2) and to (x,z)
  */
  void add3(
    long[] x3,
    long[] z3,
    long[] x2,
    long[] z2,
    long[] x1,
    long[] z1,
    long[] x,
    long[] z)
  {
    long[] t = fieldTX;
    long[] u = fieldTZ;
    long[] v = fieldUX;
    long[] w = fieldUZ;
    SubtractBigNbrModN(x2, z2, v); // v = x2-z2
    AddBigNbrModN(x1, z1, w);      // w = x1+z1
    MontgomeryMult(v, w, u);       // u = (x2-z2)*(x1+z1)
    AddBigNbrModN(x2, z2, w);      // w = x2+z2
    SubtractBigNbrModN(x1, z1, t); // t = x1-z1
    MontgomeryMult(t, w, v);       // v = (x2+z2)*(x1-z1)
    AddBigNbrModN(u, v, t);        // t = 2*(x1*x2-z1*z2)
    MontgomeryMult(t, t, w);       // w = 4*(x1*x2-z1*z2)^2
    SubtractBigNbrModN(u, v, t);   // t = 2*(x2*z1-x1*z2)
    MontgomeryMult(t, t, v);       // v = 4*(x2*z1-x1*z2)^2
    if (BigNbrAreEqual(x, x3))
    {
      System.arraycopy(x, 0, u, 0, NumberLength);
      System.arraycopy(w, 0, t, 0, NumberLength);
      MontgomeryMult(z, t, w);
      MontgomeryMult(v, u, z3);
      System.arraycopy(w, 0, x3, 0, NumberLength);
    }
    else
    {
      MontgomeryMult(w, z, x3); // x3 = 4*z*(x1*x2-z1*z2)^2
      MontgomeryMult(x, v, z3); // z3 = 4*x*(x2*z1-x1*z2)^2
    }
  }

  /* computes 2P=(x2:z2) from P=(x1:z1), with 5 mul, 4 add/sub, 5 mod.
       Uses the following global variables:
       - n : number to factor
       - b : (a+2)/4 mod n
       - u, v, w : auxiliary variables
  Modifies: x2, z2, u, v, w
  */
  void duplicate(long[] x2, long[] z2, long[] x1, long[] z1)
  {
    long[] u = fieldUZ;
    long[] v = fieldTX;
    long[] w = fieldTZ;
    AddBigNbrModN(x1, z1, w);      // w = x1+z1
    MontgomeryMult(w, w, u);       // u = (x1+z1)^2
    SubtractBigNbrModN(x1, z1, w); // w = x1-z1
    MontgomeryMult(w, w, v);       // v = (x1-z1)^2
    MontgomeryMult(u, v, x2);      // x2 = u*v = (x1^2 - z1^2)^2
    SubtractBigNbrModN(u, v, w);   // w = u-v = 4*x1*z1
    MontgomeryMult(fieldAA, w, u);
    AddBigNbrModN(u, v, u);        // u = (v+b*w)
    MontgomeryMult(w, u, z2);      // z2 = (w*u)
  }
  /* End of code "borrowed" from Paul Zimmermann's ECM4C */

  BigInteger BigIntToBigNbr(long[] GD)
  {
    byte[] Result;
    long[] Temp;
    int i, NL;
    long digit, MaxUInt = 0xFFFFFFFFl;

    Temp = new long[NumberLength];
    Convert31To32Bits(GD, Temp);
    NL = NumberLength * 4;
    Result = new byte[NL];
    for (i = 0; i < NumberLength; i++)
    {
      digit = Temp[i];
      Result[NL - 1 - 4 * i] = (byte) (digit & 0xFF);
      Result[NL - 2 - 4 * i] = (byte) (digit / 0x100 & 0xFF);
      Result[NL - 3 - 4 * i] = (byte) (digit / 0x10000 & 0xFF);
      Result[NL - 4 - 4 * i] = (byte) (digit / 0x1000000 & 0xFF);
    }
    return (new BigInteger(Result));
  }

  void LongToBigNbr(long Nbr, long Out[])
  {
    int i;

    Out[0] = Nbr & 0x7FFFFFFFl;
    Out[1] = (Nbr >> 31) & 0x7FFFFFFFl;
    for (i = 2; i < NumberLength; i++)
    {
      Out[i] = (Nbr < 0 ? 0x7FFFFFFFl : 0);
    }
  }

  boolean BigNbrIsZero(long Nbr[])
  {
    for (int i = 0; i < NumberLength; i++)
    {
      if (Nbr[i] != 0)
      {
        return false;
      }
    }
    return true;
  }

  boolean BigNbrAreEqual(long Nbr1[], long Nbr2[])
  {
    for (int i = 0; i < NumberLength; i++)
    {
      if (Nbr1[i] != Nbr2[i])
      {
        return false;
      }
    }
    return true;
  }

  void ChSignBigNbr(long Nbr[])
  {
    int NumberLength = this.NumberLength;
    long Cy = 0;
    for (int i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 31) - Nbr[i];
      Nbr[i] = Cy & 0x7FFFFFFFl;
    }
  }

  void AddBigNbr(long Nbr1[], long Nbr2[], long Sum[])
  {
    int NumberLength = this.NumberLength;
    long Cy = 0;
    for (int i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 31) + Nbr1[i] + Nbr2[i];
      Sum[i] = Cy & 0x7FFFFFFFl;
    }
  }

  void SubtractBigNbr(long Nbr1[], long Nbr2[], long Diff[])
  {
    int NumberLength = this.NumberLength;
    long Cy = 0;
    for (int i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 31) + Nbr1[i] - Nbr2[i];
      Diff[i] = Cy & 0x7FFFFFFFl;
    }
  }

  void AddBigNbr32(long Nbr1[], long Nbr2[], long Sum[])
  {
    int NumberLength = this.NumberLength;
    long Cy = 0;
    for (int i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 32) + Nbr1[i] + Nbr2[i];
      Sum[i] = Cy & 0xFFFFFFFFl;
    }
  }

  void SubtractBigNbr32(long Nbr1[], long Nbr2[], long Diff[])
  {
    int NumberLength = this.NumberLength;
    long Cy = 0;
    for (int i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 32) + Nbr1[i] - Nbr2[i];
      Diff[i] = Cy & 0xFFFFFFFFl;
    }
  }

  void MultBigNbr(long Nbr1[], long Nbr2[], long Prod[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long Cy, Pr;
    int i, j;
    Cy = Pr = 0;
    for (i = 0; i < NumberLength; i++)
    {
      Pr = Cy & MaxUInt;
      Cy >>>= 31;
      for (j = 0; j <= i; j++)
      {
        Pr += Nbr1[j] * Nbr2[i - j];
        Cy += (Pr >>> 31);
        Pr &= MaxUInt;
      }
      Prod[i] = Pr;
    }
  }

  void MultBigNbrByLong(long Nbr1[], long Nbr2, long Prod[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long Pr;
    int i, j;
    Pr = 0;
    for (i = 0; i < NumberLength; i++)
    {
      Pr = (Pr >> 31) + Nbr2 * Nbr1[i];
      Prod[i] = Pr & MaxUInt;
    }
  }

  long BigNbrModLong(long Nbr1[], long Nbr2)
  {
    int i;
    long Rem = 0;

    for (i = NumberLength - 1; i >= 0; i--)
    {
      Rem = ((Rem << 31) + Nbr1[i]) % Nbr2;
    }
    return Rem;
  }

  void AddBigNbrModN(long Nbr1[], long Nbr2[], long Sum[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long Cy = 0;
    int i;

    for (i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 31) + Nbr1[i] + Nbr2[i] - TestNbr[i];
      Sum[i] = Cy & MaxUInt;
    }
    if (Cy < 0)
    {
      Cy = 0;
      for (i = 0; i < NumberLength; i++)
      {
        Cy = (Cy >> 31) + Sum[i] + TestNbr[i];
        Sum[i] = Cy & MaxUInt;
      }
    }
  }

  void SubtractBigNbrModN(long Nbr1[], long Nbr2[], long Diff[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long Cy = 0;
    int i;

    for (i = 0; i < NumberLength; i++)
    {
      Cy = (Cy >> 31) + Nbr1[i] - Nbr2[i];
      Diff[i] = Cy & MaxUInt;
    }
    if (Cy < 0)
    {
      Cy = 0;
      for (i = 0; i < NumberLength; i++)
      {
        Cy = (Cy >> 31) + Diff[i] + TestNbr[i];
        Diff[i] = Cy & MaxUInt;
      }
    }
  }

  void MontgomeryMult(long Nbr1[], long Nbr2[], long Prod[])
  {
    int i, j, k;
    long MaxUInt = 0x7FFFFFFFl;
    long Pr, Nbr;
    long Prod0, Prod1, Prod2, Prod3, Prod4, Prod5, Prod6, Prod7;
    long Prod8, Prod9, Prod10;
    long TestNbr0,
      TestNbr1,
      TestNbr2,
      TestNbr3,
      TestNbr4,
      TestNbr5,
      TestNbr6,
      TestNbr7;
    long TestNbr8, TestNbr9, TestNbr10;
    long Nbr2_0, Nbr2_1, Nbr2_2, Nbr2_3, Nbr2_4, Nbr2_5, Nbr2_6, Nbr2_7;
    long Nbr2_8, Nbr2_9, Nbr2_10;
    long New, MontDig, Prd;
    int MontgomeryMultN = (int) this.MontgomeryMultN;
    long TestNbr[] = this.TestNbr;
    int NumberLength = this.NumberLength;
    int NumberLength1 = NumberLength - 1;
    String EcmInfo;

    if (TerminateThread)
    {
      throw new ArithmeticException();
    }
    if (lModularMult >= 0)
    {
      lModularMult++;
      if ((lModularMult % yieldFreq) == 0)
      {
        Thread.yield();
        New = System.currentTimeMillis();
        if (OldTimeElapsed >= 0
          && OldTimeElapsed / 1000 != (OldTimeElapsed + New - Old) / 1000)
        {
          OldTimeElapsed += New - Old;
          Old = New;
          switch (StepECM)
          {
            case 1 :
              EcmInfo = "Step 1: " + (indexPrimes * 100 / NbrPrimes) + "%";
              break;
            case 2 :
              EcmInfo =
                "Step 2: "
                  + (maxIndexM == 0 ? 0 : (indexM * 100 / maxIndexM))
                  + "%";
              break;
            default :
              EcmInfo = "";
          }
          labelStatus.setText(
            "Time elapsed: "
              + GetDHMS(OldTimeElapsed / 1000)
              + "    mod mult: "
              + (lModularMult >= 0 ? "" + lModularMult : "I don't know")
              + "   "
              + EcmInfo);
        }
      }
    }
    TestNbr0 = TestNbr[0];
    TestNbr1 = TestNbr[1];
    Nbr2_0 = Nbr2[0];
    Nbr2_1 = Nbr2[1];
    switch (NumberLength)
    {
      case 2 :
        Prod0 = Prod1 = 0;
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = Pr >>> 31;
          i++;
        }
        while (i < 2);
        if (Prod1 > TestNbr1 || Prod1 == TestNbr1 && (Prod0 >= TestNbr0))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = ((Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        break;

      case 3 :
        Prod0 = Prod1 = Prod2 = 0;
        TestNbr2 = TestNbr[2];
        Nbr2_2 = Nbr2[2];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = Pr >>> 31;
          i++;
        }
        while (i < 3);
        if (Prod2 > TestNbr2
          || Prod2 == TestNbr2
          && (Prod1 > TestNbr1 || Prod1 == TestNbr1 && (Prod0 >= TestNbr0)))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = ((Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        break;

      case 4 :
        Prod0 = Prod1 = Prod2 = Prod3 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = Pr >>> 31;
          i++;
        }
        while (i < 4);
        if (Prod3 > TestNbr3
          || Prod3 == TestNbr3
          && (Prod2 > TestNbr2
            || Prod2 == TestNbr2
            && (Prod1 > TestNbr1 || Prod1 == TestNbr1 && (Prod0 >= TestNbr0))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = ((Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        break;

      case 5 :
        Prod0 = Prod1 = Prod2 = Prod3 = Prod4 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = Pr >>> 31;
          i++;
        }
        while (i < 5);
        if (Prod4 > TestNbr4
          || Prod4 == TestNbr4
          && (Prod3 > TestNbr3
            || Prod3 == TestNbr3
            && (Prod2 > TestNbr2
              || Prod2 == TestNbr2
              && (Prod1 > TestNbr1 || Prod1 == TestNbr1 && (Prod0 >= TestNbr0)))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = ((Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        break;

      case 6 :
        Prod0 = Prod1 = Prod2 = Prod3 = Prod4 = Prod5 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = Pr >>> 31;
          i++;
        }
        while (i < 6);
        if (Prod5 > TestNbr5
          || Prod5 == TestNbr5
          && (Prod4 > TestNbr4
            || Prod4 == TestNbr4
            && (Prod3 > TestNbr3
              || Prod3 == TestNbr3
              && (Prod2 > TestNbr2
                || Prod2 == TestNbr2
                && (Prod1 > TestNbr1
                  || Prod1 == TestNbr1
                  && (Prod0 >= TestNbr0))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = ((Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        break;

      case 7 :
        Prod0 = Prod1 = Prod2 = Prod3 = Prod4 = Prod5 = Prod6 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = Pr >>> 31;
          i++;
        }
        while (i < 7);
        if (Prod6 > TestNbr6
          || Prod6 == TestNbr6
          && (Prod5 > TestNbr5
            || Prod5 == TestNbr5
            && (Prod4 > TestNbr4
              || Prod4 == TestNbr4
              && (Prod3 > TestNbr3
                || Prod3 == TestNbr3
                && (Prod2 > TestNbr2
                  || Prod2 == TestNbr2
                  && (Prod1 > TestNbr1
                    || Prod1 == TestNbr1
                    && (Prod0 >= TestNbr0)))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = (Pr = (Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
          Prod6 = ((Pr >> 31) + Prod6 - TestNbr6) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        break;

      case 8 :
        Prod0 = Prod1 = Prod2 = Prod3 = Prod4 = Prod5 = Prod6 = Prod7 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        TestNbr7 = TestNbr[7];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        Nbr2_7 = Nbr2[7];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr7 + Nbr * Nbr2_7 + Prod7) & 0x7FFFFFFFl;
          Prod7 = Pr >>> 31;
          i++;
        }
        while (i < 8);
        if (Prod7 > TestNbr7
          || Prod7 == TestNbr7
          && (Prod6 > TestNbr6
            || Prod6 == TestNbr6
            && (Prod5 > TestNbr5
              || Prod5 == TestNbr5
              && (Prod4 > TestNbr4
                || Prod4 == TestNbr4
                && (Prod3 > TestNbr3
                  || Prod3 == TestNbr3
                  && (Prod2 > TestNbr2
                    || Prod2 == TestNbr2
                    && (Prod1 > TestNbr1
                      || Prod1 == TestNbr1
                      && (Prod0 >= TestNbr0))))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = (Pr = (Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
          Prod6 = (Pr = (Pr >> 31) + Prod6 - TestNbr6) & MaxUInt;
          Prod7 = ((Pr >> 31) + Prod7 - TestNbr7) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        Prod[7] = Prod7;
        break;

      case 9 :
        Prod0 =
          Prod1 = Prod2 = Prod3 = Prod4 = Prod5 = Prod6 = Prod7 = Prod8 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        TestNbr7 = TestNbr[7];
        TestNbr8 = TestNbr[8];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        Nbr2_7 = Nbr2[7];
        Nbr2_8 = Nbr2[8];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr7 + Nbr * Nbr2_7 + Prod7) & 0x7FFFFFFFl;
          Prod7 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr8 + Nbr * Nbr2_8 + Prod8) & 0x7FFFFFFFl;
          Prod8 = Pr >>> 31;
          i++;
        }
        while (i < 9);
        if (Prod8 > TestNbr8
          || Prod8 == TestNbr8
          && (Prod7 > TestNbr7
            || Prod7 == TestNbr7
            && (Prod6 > TestNbr6
              || Prod6 == TestNbr6
              && (Prod5 > TestNbr5
                || Prod5 == TestNbr5
                && (Prod4 > TestNbr4
                  || Prod4 == TestNbr4
                  && (Prod3 > TestNbr3
                    || Prod3 == TestNbr3
                    && (Prod2 > TestNbr2
                      || Prod2 == TestNbr2
                      && (Prod1 > TestNbr1
                        || Prod1 == TestNbr1
                        && (Prod0 >= TestNbr0)))))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = (Pr = (Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
          Prod6 = (Pr = (Pr >> 31) + Prod6 - TestNbr6) & MaxUInt;
          Prod7 = (Pr = (Pr >> 31) + Prod7 - TestNbr7) & MaxUInt;
          Prod8 = ((Pr >> 31) + Prod8 - TestNbr8) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        Prod[7] = Prod7;
        Prod[8] = Prod8;
        break;

      case 10 :
        Prod0 =
          Prod1 =
            Prod2 = Prod3 = Prod4 = Prod5 = Prod6 = Prod7 = Prod8 = Prod9 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        TestNbr7 = TestNbr[7];
        TestNbr8 = TestNbr[8];
        TestNbr9 = TestNbr[9];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        Nbr2_7 = Nbr2[7];
        Nbr2_8 = Nbr2[8];
        Nbr2_9 = Nbr2[9];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr7 + Nbr * Nbr2_7 + Prod7) & 0x7FFFFFFFl;
          Prod7 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr8 + Nbr * Nbr2_8 + Prod8) & 0x7FFFFFFFl;
          Prod8 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr9 + Nbr * Nbr2_9 + Prod9) & 0x7FFFFFFFl;
          Prod9 = Pr >>> 31;
          i++;
        }
        while (i < 10);
        if (Prod9 > TestNbr9
          || Prod9 == TestNbr9
          && (Prod8 > TestNbr8
            || Prod8 == TestNbr8
            && (Prod7 > TestNbr7
              || Prod7 == TestNbr7
              && (Prod6 > TestNbr6
                || Prod6 == TestNbr6
                && (Prod5 > TestNbr5
                  || Prod5 == TestNbr5
                  && (Prod4 > TestNbr4
                    || Prod4 == TestNbr4
                    && (Prod3 > TestNbr3
                      || Prod3 == TestNbr3
                      && (Prod2 > TestNbr2
                        || Prod2 == TestNbr2
                        && (Prod1 > TestNbr1
                          || Prod1 == TestNbr1
                          && (Prod0 >= TestNbr0))))))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = (Pr = (Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
          Prod6 = (Pr = (Pr >> 31) + Prod6 - TestNbr6) & MaxUInt;
          Prod7 = (Pr = (Pr >> 31) + Prod7 - TestNbr7) & MaxUInt;
          Prod8 = (Pr = (Pr >> 31) + Prod8 - TestNbr8) & MaxUInt;
          Prod9 = ((Pr >> 31) + Prod9 - TestNbr9) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        Prod[7] = Prod7;
        Prod[8] = Prod8;
        Prod[9] = Prod9;
        break;

      case 11 :
        Prod0 =
          Prod1 =
            Prod2 =
              Prod3 =
                Prod4 = Prod5 = Prod6 = Prod7 = Prod8 = Prod9 = Prod10 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        TestNbr7 = TestNbr[7];
        TestNbr8 = TestNbr[8];
        TestNbr9 = TestNbr[9];
        TestNbr10 = TestNbr[10];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        Nbr2_7 = Nbr2[7];
        Nbr2_8 = Nbr2[8];
        Nbr2_9 = Nbr2[9];
        Nbr2_10 = Nbr2[10];
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr7 + Nbr * Nbr2_7 + Prod7) & 0x7FFFFFFFl;
          Prod7 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr8 + Nbr * Nbr2_8 + Prod8) & 0x7FFFFFFFl;
          Prod8 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr9 + Nbr * Nbr2_9 + Prod9) & 0x7FFFFFFFl;
          Prod9 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr10 + Nbr * Nbr2_10 + Prod10) & 0x7FFFFFFFl;
          Prod10 = Pr >>> 31;
          i++;
        }
        while (i < 11);
        if (Prod10 > TestNbr10
          || Prod10 == TestNbr10
          && (Prod9 > TestNbr9
            || Prod9 == TestNbr9
            && (Prod8 > TestNbr8
              || Prod8 == TestNbr8
              && (Prod7 > TestNbr7
                || Prod7 == TestNbr7
                && (Prod6 > TestNbr6
                  || Prod6 == TestNbr6
                  && (Prod5 > TestNbr5
                    || Prod5 == TestNbr5
                    && (Prod4 > TestNbr4
                      || Prod4 == TestNbr4
                      && (Prod3 > TestNbr3
                        || Prod3 == TestNbr3
                        && (Prod2 > TestNbr2
                          || Prod2 == TestNbr2
                          && (Prod1 > TestNbr1
                            || Prod1 == TestNbr1
                            && (Prod0 >= TestNbr0)))))))))))
        {
          Prod0 = (Pr = Prod0 - TestNbr0) & MaxUInt;
          Prod1 = (Pr = (Pr >> 31) + Prod1 - TestNbr1) & MaxUInt;
          Prod2 = (Pr = (Pr >> 31) + Prod2 - TestNbr2) & MaxUInt;
          Prod3 = (Pr = (Pr >> 31) + Prod3 - TestNbr3) & MaxUInt;
          Prod4 = (Pr = (Pr >> 31) + Prod4 - TestNbr4) & MaxUInt;
          Prod5 = (Pr = (Pr >> 31) + Prod5 - TestNbr5) & MaxUInt;
          Prod6 = (Pr = (Pr >> 31) + Prod6 - TestNbr6) & MaxUInt;
          Prod7 = (Pr = (Pr >> 31) + Prod7 - TestNbr7) & MaxUInt;
          Prod8 = (Pr = (Pr >> 31) + Prod8 - TestNbr8) & MaxUInt;
          Prod9 = (Pr = (Pr >> 31) + Prod9 - TestNbr9) & MaxUInt;
          Prod10 = ((Pr >> 31) + Prod10 - TestNbr10) & MaxUInt;
        }
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        Prod[7] = Prod7;
        Prod[8] = Prod8;
        Prod[9] = Prod9;
        Prod[10] = Prod10;
        break;

      default :
        Prod0 =
          Prod1 =
            Prod2 =
              Prod3 =
                Prod4 = Prod5 = Prod6 = Prod7 = Prod8 = Prod9 = Prod10 = 0;
        TestNbr2 = TestNbr[2];
        TestNbr3 = TestNbr[3];
        TestNbr4 = TestNbr[4];
        TestNbr5 = TestNbr[5];
        TestNbr6 = TestNbr[6];
        TestNbr7 = TestNbr[7];
        TestNbr8 = TestNbr[8];
        TestNbr9 = TestNbr[9];
        TestNbr10 = TestNbr[10];
        Nbr2_2 = Nbr2[2];
        Nbr2_3 = Nbr2[3];
        Nbr2_4 = Nbr2[4];
        Nbr2_5 = Nbr2[5];
        Nbr2_6 = Nbr2[6];
        Nbr2_7 = Nbr2[7];
        Nbr2_8 = Nbr2[8];
        Nbr2_9 = Nbr2[9];
        Nbr2_10 = Nbr2[10];
        for (j = 11; j < NumberLength; j++)
        {
          Prod[j] = 0;
        }
        i = 0;
        do
        {
          Pr = (Nbr = Nbr1[i]) * Nbr2_0 + Prod0;
          MontDig = ((int) Pr * MontgomeryMultN) & MaxUInt;
          Prod0 = (Pr = ((MontDig * TestNbr0 + Pr) >>> 31) +
                  MontDig * TestNbr1 + Nbr * Nbr2_1 + Prod1) & 0x7FFFFFFFl;
          Prod1 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr2 + Nbr * Nbr2_2 + Prod2) & 0x7FFFFFFFl;
          Prod2 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr3 + Nbr * Nbr2_3 + Prod3) & 0x7FFFFFFFl;
          Prod3 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr4 + Nbr * Nbr2_4 + Prod4) & 0x7FFFFFFFl;
          Prod4 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr5 + Nbr * Nbr2_5 + Prod5) & 0x7FFFFFFFl;
          Prod5 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr6 + Nbr * Nbr2_6 + Prod6) & 0x7FFFFFFFl;
          Prod6 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr7 + Nbr * Nbr2_7 + Prod7) & 0x7FFFFFFFl;
          Prod7 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr8 + Nbr * Nbr2_8 + Prod8) & 0x7FFFFFFFl;
          Prod8 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr9 + Nbr * Nbr2_9 + Prod9) & 0x7FFFFFFFl;
          Prod9 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr10 + Nbr * Nbr2_10 + Prod10) & 0x7FFFFFFFl;
          Prod10 = (Pr = (Pr >>> 31) +
                  MontDig * TestNbr[11] + Nbr * Nbr2[11] + Prod[11]) & 0x7FFFFFFFl;
          for (j = 12; j < NumberLength; j++)
          {
            Prod[j-1] = (Pr = (Pr >>> 31) +
                   MontDig * TestNbr[j] + Nbr * Nbr2[j] + Prod[j]) & 0x7FFFFFFFl;
          }
          Prod[j - 1] = (Pr >>> 31);
          i++;
        }
        while (i < NumberLength);
        Prod[0] = Prod0;
        Prod[1] = Prod1;
        Prod[2] = Prod2;
        Prod[3] = Prod3;
        Prod[4] = Prod4;
        Prod[5] = Prod5;
        Prod[6] = Prod6;
        Prod[7] = Prod7;
        Prod[8] = Prod8;
        Prod[9] = Prod9;
        Prod[10] = Prod10;
        for (j = NumberLength - 1; j >= 0; j--)
        {
          if (Prod[j] != TestNbr[j])
          {
            break;
          }
        }
        if (j < 0 || j >= 0 && Prod[j] >= TestNbr[j])
        {
          Pr = 0;
          for (j = 0; j < NumberLength; j++)
          {
            Prod[j] = (Pr = (Pr >> 31) + Prod[j] - TestNbr[j]) & MaxUInt;
          }
        }
    } /* end switch */
  }

  void GetMontgomeryParms()
  {
    int NumberLength = this.NumberLength;
    int N, x, j;

    dN = (double) TestNbr[NumberLength - 1];
    if (NumberLength > 1)
    {
      dN += (double) TestNbr[NumberLength - 2] / dDosALa31;
    }
    if (NumberLength > 2)
    {
      dN += (double) TestNbr[NumberLength - 3] / dDosALa62;
    }

    x = N = (int) TestNbr[0]; // 2 least significant bits of inverse correct.
    x = x * (2 - N * x); // 4 least significant bits of inverse correct.
    x = x * (2 - N * x); // 8 least significant bits of inverse correct.
    x = x * (2 - N * x); // 16 least significant bits of inverse correct.
    x = x * (2 - N * x); // 32 least significant bits of inverse correct.
    MontgomeryMultN = (-x) & 0x7FFFFFFF;
    j = NumberLength;
    MontgomeryMultR1[j] = 1;
    do
    {
      MontgomeryMultR1[--j] = 0;
    }
    while (j > 0);
    AdjustModN(MontgomeryMultR1);
    MultBigNbrModN(MontgomeryMultR1, MontgomeryMultR1, MontgomeryMultR2);
    MontgomeryMult(MontgomeryMultR2, MontgomeryMultR2, MontgomeryMultAfterInv);
    AddBigNbrModN(MontgomeryMultR1, MontgomeryMultR1, MontgomeryMultR2);
  }

  void BigNbrModN(long Nbr[], int Length, long Mod[])
  {
    int i, j;
    for (i = 0; i < NumberLength; i++)
    {
      Mod[i] = Nbr[i + Length - NumberLength];
    }
    Mod[i] = 0;
    AdjustModN(Mod);
    for (i = Length - NumberLength - 1; i >= 0; i--)
    {
      for (j = NumberLength; j > 0; j--)
      {
        Mod[j] = Mod[j - 1];
      }
      Mod[0] = Nbr[i];
      AdjustModN(Mod);
    }
  }

  void MultBigNbrModN(long Nbr1[], long Nbr2[], long Prod[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    int i, j;
    long Pr, Nbr;

    i = NumberLength;
    do
    {
      Prod[--i] = 0;
    }
    while (i > 0);
    i = NumberLength;
    do
    {
      Nbr = Nbr1[--i];
      j = NumberLength;
      do
      {
        Prod[j] = Prod[j - 1];
        j--;
      }
      while (j > 0);
      Prod[0] = 0;
      Pr = 0;
      for (j = 0; j < NumberLength; j++)
      {
        Pr = (Pr >>> 31) + Nbr * Nbr2[j] + Prod[j];
        Prod[j] = Pr & MaxUInt;
      }
      Prod[j] += (Pr >>> 31);
      AdjustModN(Prod);
    }
    while (i > 0);
  }

  void MultBigNbrByLongModN(long Nbr1[], long Nbr2, long Prod[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long Pr;
    int j;

    Pr = 0;
    for (j = 0; j < NumberLength; j++)
    {
      Pr = (Pr >>> 31) + Nbr2 * Nbr1[j];
      Prod[j] = Pr & MaxUInt;
    }
    Prod[j] = (Pr >>> 31);
    AdjustModN(Prod);
  }

  void AdjustModN(long Nbr[])
  {
    int NumberLength = this.NumberLength;
    long MaxUInt = 0x7FFFFFFFl;
    long TrialQuotient;
    long Cy;
    int i;
    double dAux;

    dAux =
      (double) Nbr[NumberLength] * dDosALa31 + (double) Nbr[NumberLength - 1];
    if (NumberLength > 1)
    {
      dAux += (double) Nbr[NumberLength - 2] / dDosALa31;
    }
    TrialQuotient = (long) Math.ceil(dAux / dN) + 2;
    if (TrialQuotient >= DosALa32)
    {
      Cy = 0;
      for (i = 0; i < NumberLength; i++)
      {
        Cy = Nbr[i + 1] - (TrialQuotient >>> 31) * TestNbr[i] - Cy;
        Nbr[i + 1] = Cy & MaxUInt;
        Cy = (MaxUInt - Cy) >>> 31;
      }
      TrialQuotient &= MaxUInt;
    }
    Cy = 0;
    for (i = 0; i < NumberLength; i++)
    {
      Cy = Nbr[i] - TrialQuotient * TestNbr[i] - Cy;
      Nbr[i] = Cy & MaxUInt;
      Cy = (MaxUInt - Cy) >>> 31;
    }
    Nbr[NumberLength] -= Cy;
    while ((Nbr[NumberLength] & MaxUInt) != 0)
    {
      Cy = 0;
      for (i = 0; i < NumberLength; i++)
      {
        Cy += Nbr[i] + TestNbr[i];
        Nbr[i] = Cy & MaxUInt;
        Cy >>= 31;
      }
      Nbr[NumberLength] += Cy;
    }
  }

  void DivBigNbrByLong(long Dividend[], long Divisor, long Quotient[])
  {
    int i;
    boolean ChSignDivisor = false;
    long Divid, Rem = 0;

    if (Divisor < 0)
    {                            // If divisor is negative...
      ChSignDivisor = true;      // Indicate to change sign at the end and
      Divisor = -Divisor;        // convert divisor to positive.
    }
    if (Dividend[i = NumberLength - 1] >= 0x40000000l)
    {                            // If dividend is negative...
      Rem = Divisor - 1;
    }
    for ( ; i >= 0; i--)
    {
      Divid = Dividend[i] + (Rem << 31);
      Rem = Divid % Divisor;
      Quotient[i] = Divid / Divisor;
    }
    if (ChSignDivisor)
    {                            // Change sign if divisor is negative.
      ChSignBigNbr(Quotient);    // Convert divisor to positive.
    }
  }

  long RemDivBigNbrByLong(long Dividend[], long Divisor)
  {
    int i;
    long Divid, Rem = 0;

    if (Divisor < 0)
    {                            // If divisor is negative...
      Divisor = -Divisor;        // Convert divisor to positive.
    }
    if (Dividend[i = NumberLength - 1] >= 0x40000000l)
    {                            // If dividend is negative...
      Rem = Divisor - 1;
    }
    for ( ; i >= 0; i--)
    {
      Divid = Dividend[i] + (Rem << 31);
      Rem = Divid % Divisor;
    }
    return Rem;
  }

  // Gcd calculation:
  // Step 1: Set k<-0, and then repeatedly set k<-k+1, u<-u/2, v<-v/2
  //         zero or more times until u and v are not both even.
  // Step 2: If u is odd, set t<-(-v) and go to step 4. Otherwise set t<-u.
  // Step 3: Set t<-t/2
  // Step 4: If t is even, go back to step 3.
  // Step 5: If t>0, set u<-t, otherwise set v<-(-t).
  // Step 6: Set t<-u-v. If t!=0, go back to step 3.
  // Step 7: The GCD is u*2^k.
  void GcdBigNbr(long Nbr1[], long Nbr2[], long Gcd[])
  {
    int i, k;
    int NumberLength = this.NumberLength;

    System.arraycopy(Nbr1, 0, CalcAuxGcdU, 0, NumberLength);
    System.arraycopy(Nbr2, 0, CalcAuxGcdV, 0, NumberLength);
    for (i = 0; i < NumberLength; i++)
    {
      if (CalcAuxGcdU[i] != 0)
      {
        break;
      }
    }
    if (i == NumberLength)
    {
      System.arraycopy(CalcAuxGcdV, 0, Gcd, 0, NumberLength);
      return;
    }
    for (i = 0; i < NumberLength; i++)
    {
      if (CalcAuxGcdV[i] != 0)
      {
        break;
      }
    }
    if (i == NumberLength)
    {
      System.arraycopy(CalcAuxGcdU, 0, Gcd, 0, NumberLength);
      return;
    }
    if (CalcAuxGcdU[NumberLength - 1] >= 0x40000000l)
    {
      ChSignBigNbr(CalcAuxGcdU);
    }
    if (CalcAuxGcdV[NumberLength - 1] >= 0x40000000l)
    {
      ChSignBigNbr(CalcAuxGcdV);
    }
    k = 0;
    while ((CalcAuxGcdU[0] & 1) == 0 && (CalcAuxGcdV[0] & 1) == 0)
    { // Step 1
      k++;
      DivBigNbrByLong(CalcAuxGcdU, 2, CalcAuxGcdU);
      DivBigNbrByLong(CalcAuxGcdV, 2, CalcAuxGcdV);
    }
    if ((CalcAuxGcdU[0] & 1) == 1)
    { // Step 2
      System.arraycopy(CalcAuxGcdV, 0, CalcAuxGcdT, 0, NumberLength);
      ChSignBigNbr(CalcAuxGcdT);
    }
    else
    {
      System.arraycopy(CalcAuxGcdU, 0, CalcAuxGcdT, 0, NumberLength);
    }
    do
    {
      while ((CalcAuxGcdT[0] & 1) == 0)
      { // Step 4
        DivBigNbrByLong(CalcAuxGcdT, 2, CalcAuxGcdT); // Step 3
      }
      if (CalcAuxGcdT[NumberLength - 1] < 0x40000000l)
      { // Step 5
        System.arraycopy(CalcAuxGcdT, 0, CalcAuxGcdU, 0, NumberLength);
      }
      else
      {
        System.arraycopy(CalcAuxGcdT, 0, CalcAuxGcdV, 0, NumberLength);
        ChSignBigNbr(CalcAuxGcdV);
      }
      SubtractBigNbr(CalcAuxGcdU, CalcAuxGcdV, CalcAuxGcdT); // Step 6
      for (i = 0; i < NumberLength; i++)
      {
        if (CalcAuxGcdT[i] != 0)
        {
          break;
        }
      }
    }
    while (i != NumberLength);
    System.arraycopy(CalcAuxGcdU, 0, Gcd, 0, NumberLength); // Step 7
    while (k > 0)
    {
      AddBigNbr(Gcd, Gcd, Gcd);
      k--;
    }
  }

  String BigNbrToString(long Nbr[])
  {
    long Rem;
    int i;
    boolean ChSign = false;
    String nbrOutput = "";

    if (Nbr[NumberLength - 1] >= 0x40000000)
    {
      ChSignBigNbr(Nbr);
      ChSign = true;
    }
    System.arraycopy(Nbr, 0, CalcBigNbr, 0, NumberLength);
    do
    {
      Rem = 0;
      for (i = NumberLength - 1; i >= 0; i--)
      {
        CalcBigNbr[i] += Rem << 31;
        Rem = CalcBigNbr[i] % Mi;
        CalcBigNbr[i] /= Mi;
      }
      nbrOutput = ((Rem + Mi) + "").substring(1) + nbrOutput;
      for (i = 0; i < NumberLength; i++)
      {
        if (CalcBigNbr[i] != 0)
        {
          break;
        }
      }
    }
    while (i < NumberLength);
    while (nbrOutput.charAt(0) == '0' && nbrOutput.length() > 1)
    {
      nbrOutput = nbrOutput.substring(1);
    }
    if (ChSign)
    {
      ChSignBigNbr(Nbr);
      nbrOutput = "-" + nbrOutput;
    }
    return nbrOutput;
  }

  void Convert31To32Bits(long [] nbr31bits, long [] nbr32bits)
  {
    int i, j, k;
    i = 0;
    for (j = -1; j < NumberLength; j++)
    {
      k = i%31;
      if (k == 0)
      {
        j++;
      }
      if (j == NumberLength)
      {
        break;
      }
      if (j == NumberLength-1)
      {
        nbr32bits[i] = nbr31bits[j] >> k;
      }
      else
      {
        nbr32bits[i] = ((nbr31bits[j] >> k) |
                        (nbr31bits[j+1] << (31-k))) & 0xFFFFFFFFl;
      }
      i++;
    }
    for (; i<NumberLength; i++)
    {
      nbr32bits[i] = 0;
    }
  }

  void Convert32To31Bits(long [] nbr32bits, long [] nbr31bits)
  {
    int i, j, k;
    j = 0;
    nbr32bits[NumberLength] = 0;
    for (i = 0; i < NumberLength; i++)
    {
      k = i%32;
      if (k == 0)
      {
        nbr31bits[i] = nbr32bits[j] & 0x7FFFFFFFl;
      }
      else
      {
        nbr31bits[i] = ((nbr32bits[j] >> (32-k)) |
                        (nbr32bits[j+1] << k)) & 0x7FFFFFFFl;
        j++;
      }
    }
  }

  /***********************************************************************/
  /* NAME: ModInvBigNbr                                                  */
  /*                                                                     */
  /* PURPOSE: Find the inverse multiplicative modulo v.                  */
  /*                                                                     */
  /* The algorithm terminates with u1 = u^(-1) MOD v.                    */
  /***********************************************************************/
  void ModInvBigNbr(long[] a, long[] inv, long[] b)
  {
    int i, j;
    int NumberLength = this.NumberLength;
    int Dif, E;
    int st1, st2;
    long Yaa, Yab; // 2^E * A'     = Yaa A + Yab B
    long Yba, Ybb; // 2^E * B'     = Yba A + Ybb B
    long Ygb0; // 2^E * Mu'    = Yaa Mu + Yab Gamma + Ymb0 B0
    long Ymb0; // 2^E * Gamma' = Yba Mu + Ybb Gamma + Ygb0 B0
    int Iaa, Iab, Iba, Ibb;
    long Tmp1, Tmp2, Tmp3, Tmp4, Tmp5;
    int B0l;
    int invB0l;
    int Al, Bl, T1, Gl, Ml, P;
    long T;
    long Cy1, Cy2, Cy3, Cy4;
    int Yaah, Yabh, Ybah, Ybbh;
    int Ymb0h, Ygb0h;
    long Pr1, Pr2, Pr3, Pr4, Pr5, Pr6, Pr7;
    long[] B = this.biTmp;
    long[] CalcAuxModInvA = this.CalcAuxGcdU;
    long[] CalcAuxModInvB = this.CalcAuxGcdV;
    long[] CalcAuxModInvMu = this.CalcAuxGcdT;   // Cannot be inv
    long[] CalcAuxModInvGamma = inv;

    Convert31To32Bits(a, CalcAuxModInvA);
    Convert31To32Bits(b, CalcAuxModInvB);
    System.arraycopy(CalcAuxModInvB, 0, B, 0, NumberLength);
    B0l = (int)B[0];
    invB0l = B0l; // 2 least significant bits of inverse correct.
    invB0l = invB0l * (2 - B0l * invB0l); // 4 LSB of inverse correct.
    invB0l = invB0l * (2 - B0l * invB0l); // 8 LSB of inverse correct.
    invB0l = invB0l * (2 - B0l * invB0l); // 16 LSB of inverse correct.
    invB0l = invB0l * (2 - B0l * invB0l); // 32 LSB of inverse correct.
    for (i = NumberLength - 1; i >= 0; i--)
    {
      CalcAuxModInvGamma[i] = 0;
      CalcAuxModInvMu[i] = 0;
    }
    CalcAuxModInvMu[0] = 1;
    Dif = 0;
    outer_loop : do
    {
      Iaa = Ibb = 1;
      Iab = Iba = 0;
      Al = (int) CalcAuxModInvA[0];
      Bl = (int) CalcAuxModInvB[0];
      E = 0;
      P = 1;
      if (Bl == 0)
      {
        for (i = NumberLength - 1; i >= 0; i--)
        {
          if (CalcAuxModInvB[i] != 0)
            break;
        }
        if (i < 0)
          break; // Go out of loop if CalcAuxModInvB = 0
      }
      do
      {
        T1 = 0;
        while ((Bl & 1) == 0)
        {
          if (E == 31)
          {
            Yaa = Iaa;
            Yab = Iab;
            Yba = Iba;
            Ybb = Ibb;
            Gl = (int) CalcAuxModInvGamma[0];
            Ml = (int) CalcAuxModInvMu[0];
            Dif++;
            T1++;
            Yaa <<= T1;
            Yab <<= T1;
            Ymb0 = (- (int) Yaa * Ml - (int) Yab * Gl) * invB0l;
            Ygb0 = (-Iba * Ml - Ibb * Gl) * invB0l;
            Cy1 = Cy2 = Cy3 = Cy4 = 0;
            Yaah = (int) (Yaa >> 32);
            Yabh = (int) (Yab >> 32);
            Ybah = (int) (Yba >> 32);
            Ybbh = (int) (Ybb >> 32);
            Ymb0h = (int) (Ymb0 >> 32);
            Ygb0h = (int) (Ygb0 >> 32);
            Yaa &= 0xFFFFFFFFL;
            Yab &= 0xFFFFFFFFL;
            Yba &= 0xFFFFFFFFL;
            Ybb &= 0xFFFFFFFFL;
            Ymb0 &= 0xFFFFFFFFL;
            Ygb0 &= 0xFFFFFFFFL;

            st1 = Yaah * 6 + Yabh * 2 + Ymb0h;
            st2 = Ybah * 6 + Ybbh * 2 + Ygb0h;
            for (i = 0; i < NumberLength; i++)
            {
              Pr1 = Yaa * (Tmp1 = CalcAuxModInvMu[i]);
              Pr2 = Yab * (Tmp2 = CalcAuxModInvGamma[i]);
              Pr3 = Ymb0 * (Tmp3 = B[i]);
              Pr4 =
                (Pr1 & 0xFFFFFFFFL)
                  + (Pr2 & 0xFFFFFFFFL)
                  + (Pr3 & 0xFFFFFFFFL)
                  + Cy3;
              Pr5 = Yaa * (Tmp4 = CalcAuxModInvA[i]);
              Pr6 = Yab * (Tmp5 = CalcAuxModInvB[i]);
              Pr7 = (Pr5 & 0xFFFFFFFFL) + (Pr6 & 0xFFFFFFFFL) + Cy1;
              switch (st1)
              {
                case -9 :
                  Cy3 = -Tmp1 - Tmp2 - Tmp3;
                  Cy1 = -Tmp4 - Tmp5;
                  break;
                case -8 :
                  Cy3 = -Tmp1 - Tmp2;
                  Cy1 = -Tmp4 - Tmp5;
                  break;
                case -7 :
                  Cy3 = -Tmp1 - Tmp3;
                  Cy1 = -Tmp4;
                  break;
                case -6 :
                  Cy3 = -Tmp1;
                  Cy1 = -Tmp4;
                  break;
                case -5 :
                  Cy3 = -Tmp1 + Tmp2 - Tmp3;
                  Cy1 = -Tmp4 + Tmp5;
                  break;
                case -4 :
                  Cy3 = -Tmp1 + Tmp2;
                  Cy1 = -Tmp4 + Tmp5;
                  break;
                case -3 :
                  Cy3 = -Tmp2 - Tmp3;
                  Cy1 = -Tmp5;
                  break;
                case -2 :
                  Cy3 = -Tmp2;
                  Cy1 = -Tmp5;
                  break;
                case -1 :
                  Cy3 = -Tmp3;
                  Cy1 = 0;
                  break;
                case 0 :
                  Cy3 = 0;
                  Cy1 = 0;
                  break;
                case 1 :
                  Cy3 = Tmp2 - Tmp3;
                  Cy1 = Tmp5;
                  break;
                case 2 :
                  Cy3 = Tmp2;
                  Cy1 = Tmp5;
                  break;
                case 3 :
                  Cy3 = Tmp1 - Tmp2 - Tmp3;
                  Cy1 = Tmp4 - Tmp5;
                  break;
                case 4 :
                  Cy3 = Tmp1 - Tmp2;
                  Cy1 = Tmp4 - Tmp5;
                  break;
                case 5 :
                  Cy3 = Tmp1 - Tmp3;
                  Cy1 = Tmp4;
                  break;
                case 6 :
                  Cy3 = Tmp1;
                  Cy1 = Tmp4;
                  break;
                case 7 :
                  Cy3 = Tmp1 + Tmp2 - Tmp3;
                  Cy1 = Tmp4 + Tmp5;
                  break;
                case 8 :
                  Cy3 = Tmp1 + Tmp2;
                  Cy1 = Tmp4 + Tmp5;
                  break;
              }
              Cy3 += (Pr1 >>> 32) + (Pr2 >>> 32) + (Pr3 >>> 32) + (Pr4 >> 32);
              Cy1 += (Pr5 >>> 32) + (Pr6 >>> 32) + (Pr7 >> 32);
              if (i > 0)
              {
                CalcAuxModInvMu[i - 1] = Pr4 & 0xFFFFFFFFL;
                CalcAuxModInvA[i - 1] = Pr7 & 0xFFFFFFFFL;
              }
              Pr1 = Yba * Tmp1;
              Pr2 = Ybb * Tmp2;
              Pr3 = Ygb0 * Tmp3;
              Pr4 =
                (Pr1 & 0xFFFFFFFFL)
                  + (Pr2 & 0xFFFFFFFFL)
                  + (Pr3 & 0xFFFFFFFFL)
                  + Cy4;
              Pr5 = Yba * Tmp4;
              Pr6 = Ybb * Tmp5;
              Pr7 = (Pr5 & 0xFFFFFFFFL) + (Pr6 & 0xFFFFFFFFL) + Cy2;
              switch (st2)
              {
                case -9 :
                  Cy4 = -Tmp1 - Tmp2 - Tmp3;
                  Cy2 = -Tmp4 - Tmp5;
                  break;
                case -8 :
                  Cy4 = -Tmp1 - Tmp2;
                  Cy2 = -Tmp4 - Tmp5;
                  break;
                case -7 :
                  Cy4 = -Tmp1 - Tmp3;
                  Cy2 = -Tmp4;
                  break;
                case -6 :
                  Cy4 = -Tmp1;
                  Cy2 = -Tmp4;
                  break;
                case -5 :
                  Cy4 = -Tmp1 + Tmp2 - Tmp3;
                  Cy2 = -Tmp4 + Tmp5;
                  break;
                case -4 :
                  Cy4 = -Tmp1 + Tmp2;
                  Cy2 = -Tmp4 + Tmp5;
                  break;
                case -3 :
                  Cy4 = -Tmp2 - Tmp3;
                  Cy2 = -Tmp5;
                  break;
                case -2 :
                  Cy4 = -Tmp2;
                  Cy2 = -Tmp5;
                  break;
                case -1 :
                  Cy4 = -Tmp3;
                  Cy2 = 0;
                  break;
                case 0 :
                  Cy4 = 0;
                  Cy2 = 0;
                  break;
                case 1 :
                  Cy4 = Tmp2 - Tmp3;
                  Cy2 = Tmp5;
                  break;
                case 2 :
                  Cy4 = Tmp2;
                  Cy2 = Tmp5;
                  break;
                case 3 :
                  Cy4 = Tmp1 - Tmp2 - Tmp3;
                  Cy2 = Tmp4 - Tmp5;
                  break;
                case 4 :
                  Cy4 = Tmp1 - Tmp2;
                  Cy2 = Tmp4 - Tmp5;
                  break;
                case 5 :
                  Cy4 = Tmp1 - Tmp3;
                  Cy2 = Tmp4;
                  break;
                case 6 :
                  Cy4 = Tmp1;
                  Cy2 = Tmp4;
                  break;
                case 7 :
                  Cy4 = Tmp1 + Tmp2 - Tmp3;
                  Cy2 = Tmp4 + Tmp5;
                  break;
                case 8 :
                  Cy4 = Tmp1 + Tmp2;
                  Cy2 = Tmp4 + Tmp5;
                  break;
              }
              Cy4 += (Pr1 >>> 32) + (Pr2 >>> 32) + (Pr3 >>> 32) + (Pr4 >> 32);
              Cy2 += (Pr5 >>> 32) + (Pr6 >>> 32) + (Pr7 >> 32);
              if (i > 0)
              {
                CalcAuxModInvGamma[i - 1] = Pr4 & 0xFFFFFFFFL;
                CalcAuxModInvB[i - 1] = Pr7 & 0xFFFFFFFFL;
              }
            }

            if ((int) CalcAuxModInvA[i - 1] < 0)
            {
              Cy1 -= Yaa;
              Cy2 -= Yba;
            }
            if ((int) CalcAuxModInvB[i - 1] < 0)
            {
              Cy1 -= Yab;
              Cy2 -= Ybb;
            }
            if ((int) CalcAuxModInvMu[i - 1] < 0)
            {
              Cy3 -= Yaa;
              Cy4 -= Yba;
            }
            if ((int) CalcAuxModInvGamma[i - 1] < 0)
            {
              Cy3 -= Yab;
              Cy4 -= Ybb;
            }
            CalcAuxModInvA[i - 1] = Cy1 & 0xFFFFFFFFL;
            CalcAuxModInvB[i - 1] = Cy2 & 0xFFFFFFFFL;
            CalcAuxModInvMu[i - 1] = Cy3 & 0xFFFFFFFFL;
            CalcAuxModInvGamma[i - 1] = Cy4 & 0xFFFFFFFFL;
            continue outer_loop;
          }
          Bl >>= 1;
          Dif++;
          E++;
          P *= 2;
          T1++;
        }; /* end while */
        Iaa <<= T1;
        Iab <<= T1;
        if (Dif >= 0)
        {
          Dif = -Dif;
          if (((Al + Bl) & 3) == 0)
          {
            T1 = Iba;
            Iba += Iaa;
            Iaa = T1;
            T1 = Ibb;
            Ibb += Iab;
            Iab = T1;
            T1 = Bl;
            Bl += Al;
            Al = T1;
          }
          else
          {
            T1 = Iba;
            Iba -= Iaa;
            Iaa = T1;
            T1 = Ibb;
            Ibb -= Iab;
            Iab = T1;
            T1 = Bl;
            Bl -= Al;
            Al = T1;
          }
        }
        else
        {
          if (((Al + Bl) & 3) == 0)
          {
            Iba += Iaa;
            Ibb += Iab;
            Bl += Al;
          }
          else
          {
            Iba -= Iaa;
            Ibb -= Iab;
            Bl -= Al;
          }
        }
        Dif--;
      }
      while (true);
    }
    while (true);
    if (CalcAuxModInvA[0] != 1)
    {
      SubtractBigNbr32(B, CalcAuxModInvMu, CalcAuxModInvMu);
    }
    if ((int) CalcAuxModInvMu[i = NumberLength - 1] < 0)
    {
      AddBigNbr32(B, CalcAuxModInvMu, CalcAuxModInvMu);
    }
    for (; i >= 0; i--)
    {
      if (B[i] != CalcAuxModInvMu[i])
        break;
    }
    if (i < 0 || B[i] < CalcAuxModInvMu[i])
    { // If B < Mu
      SubtractBigNbr32(CalcAuxModInvMu, B, CalcAuxModInvMu); // Mu <- Mu - B
    }
    Convert32To31Bits(CalcAuxModInvMu, inv);
  }

  void FactorFibonacci(int Index, BigInteger BigOriginal)
  {
    int Index2, k;
    BigInteger Nro1;
    if (onlyFactoring)
    {
      NroFact = 1;
      Factores[0] = BigOriginal;
      Index2 = Index;
      while (Index2 % 2 == 0)
      {
        Index2 /= 2;
      }
      k = 1; /* Factor F(Index2) */
      while (k * k <= Index2)
      {
        if (Index2 % k == 0)
        {
          Nro1 = Fibonacci(k).gcd(BigOriginal);
          InsertFactor(Nro1);
          InsertFactor(BigOriginal.divide(Nro1));
          Nro1 = Fibonacci(Index / k).gcd(BigOriginal);
          InsertFactor(Nro1);
          InsertFactor(BigOriginal.divide(Nro1));
        }
        k += 2;
      }
      Index2 = Index;
      while (Index2 % 2 == 0)
      {
        Index2 /= 2;
        InsertLucasFactor(Index2, BigOriginal);
      }
      SortFactors();
    }
  }

  void FactorLucas(int Index, BigInteger BigOriginal)
  {
    NroFact = 1;
    if (onlyFactoring)
    {
      Factores[0] = BigOriginal;
      InsertLucasFactor(Index, BigOriginal);
      SortFactors();
    }
  }

  void InsertLucasFactor(int Index, BigInteger BigOriginal)
  {
    int k;
    if (onlyFactoring)
    {
      BigInteger Fibo = BigInt0;
      BigInteger BigInt5 = BigInteger.valueOf(5);
      BigInteger Nro1;
      k = 1; /* Factor L(Index) */
      while (k * k <= Index)
      {
        if (Index % k == 0)
        {
          Nro1 = Lucas(k).gcd(BigOriginal);
          InsertFactor(Nro1);
          if (k % 5 == 0)
          {
            Fibo = Fibonacci(k);
            InsertFactor(
              BigInt5.multiply(Fibo).subtract(BigInt5).multiply(Fibo).add(
                BigInt1));
            InsertFactor(
              BigInt5.multiply(Fibo).add(BigInt5).multiply(Fibo).add(BigInt1));
          }
          else
          {
            InsertFactor(Nro1);
            InsertFactor(BigOriginal.divide(Nro1));
          }
          Nro1 = Lucas(Index / k).gcd(BigOriginal);
          InsertFactor(Nro1);
          if ((Index / k) % 5 == 0)
          {
            Fibo = Fibonacci(Index / k);
            InsertFactor(
              BigInt5.multiply(Fibo).subtract(BigInt5).multiply(Fibo).add(
                BigInt1));
            InsertFactor(
              BigInt5.multiply(Fibo).add(BigInt5).multiply(Fibo).add(BigInt1));
          }
          else
          {
            InsertFactor(Nro1);
            InsertFactor(BigOriginal.divide(Nro1));
          }
        }
        k++;
      }
    }
  }

  BigInteger Fibonacci(int Index)
  {
    int i;
    if (onlyFactoring)
    {
      BigInteger FibonPrev = BigInt1;
      BigInteger FibonAct = BigInt0;
      BigInteger FibonNext;
      for (i = 1; i <= Index; i++)
      {
        FibonNext = FibonPrev.add(FibonAct);
        FibonPrev = FibonAct;
        FibonAct = FibonNext;
      }
      return FibonAct;
    }
    else
    {
      return null;
    }
  }

  BigInteger Lucas(int Index)
  {
    int i;
    if (onlyFactoring)
    {
      BigInteger LucasPrev = BigInteger.valueOf(-1);
      BigInteger LucasAct = BigInt2;
      BigInteger LucasNext;
      for (i = 1; i <= Index; i++)
      {
        LucasNext = LucasPrev.add(LucasAct);
        LucasPrev = LucasAct;
        LucasAct = LucasNext;
      }
      return LucasAct;
    }
    else
    {
      return null;
    }
  }

  void Cunningham(
    BigInteger BigBase,
    int Expon,
    BigInteger BigIncre,
    BigInteger BigOriginal)
  {
    int Expon2, k;
    int Incre;
    BigInteger Nro1;

    if (onlyFactoring)
    {
      Incre = BigIncre.intValue();
      NroFact = 1;
      Factores[0] = BigOriginal;
      Expon2 = Expon;
      Nro1 = BigBase.pow(Expon2).add(BigIncre);
      InsertFactor(Nro1);
      if ((digitsInGroup & 0x1000) == 0 && Nro1.bitLength() > 60)
      {
        try
        { // Get known primitive factors.
          lowerTextArea.setText(
            "Requesting known primitive factors from Web server.");
          URL script =
            new URL(
              getDocumentBase(),
              "factors.pl?base="
                + BigBase.intValue()
                + "&expon="
                + Expon
                + "&type="
                + (Incre == 1 ? "p" : "m"));
          BufferedInputStream buffer =
            new BufferedInputStream(script.openStream());
          DataInputStream in = new DataInputStream(buffer);
          String factorsAscii = in.readLine();
          in.close();
          if (factorsAscii.length() > 0)
          { // Factors found in server.
            int indexFactors = 0;
            int newIndexFactors = 0;
            do
            { // Loop through factors.
              newIndexFactors = factorsAscii.indexOf("*", indexFactors);
              if (newIndexFactors > 0)
              {
                InsertFactor(
                  new BigInteger(
                    factorsAscii.substring(indexFactors, newIndexFactors)));
              }
              else
              {
                InsertFactor(
                  new BigInteger(factorsAscii.substring(indexFactors)));
              }
              indexFactors = newIndexFactors + 1;
            }
            while (indexFactors > 0);
          }
        }
        catch (Exception e)
        {
        };
      }
      while (Expon2 % 2 == 0 && Incre == -1)
      {
        Expon2 /= 2;
        InsertFactor(BigBase.pow(Expon2).add(BigInt1));
        InsertAurifFactors(BigBase, Expon2, 1);
      }
      k = 1;
      while (k * k <= Expon)
      {
        if (Expon % k == 0)
        {
          if (k % 2 != 0)
          { /* Only for odd exponent */
            Nro1 = BigBase.pow(Expon / k).add(BigIncre).gcd(BigOriginal);
            InsertFactor(Nro1);
            InsertFactor(BigOriginal.divide(Nro1));
            InsertAurifFactors(BigBase, Expon / k, Incre);
          }
          if ((Expon / k) % 2 != 0)
          { /* Only for odd exponent */
            Nro1 = BigBase.pow(k).add(BigIncre).gcd(BigOriginal);
            InsertFactor(Nro1);
            InsertFactor(BigOriginal.divide(Nro1));
            InsertAurifFactors(BigBase, k, Incre);
          }
        }
        k++;
      }
      SortFactors();
    }
  }

  void InsertAurifFactors(BigInteger BigBase, int Expon, int Incre)
  {
    int t1, t2, t3, N, N1, S, S1, q, L, j, k, Base;
    if (BigBase.compareTo(BigInteger.valueOf(386)) <= 0)
    {
      Base = BigBase.intValue();
      if (Expon % 2 == 0 && Incre == -1)
      {
        do
        {
          Expon /= 2;
        }
        while (Expon % 2 == 0);
        Incre = Base % 4 - 2;
      }
      if (Expon % Base == 0
        && Expon / Base % 2 != 0
        && ((Base % 4 != 1 && Incre == 1) || (Base % 4 == 1 && Incre == -1)))
      {
        N = Base;
        if (N % 4 == 1)
        {
          N1 = N;
        }
        else
        {
          N1 = 2 * N;
        }
        if (N % 4 == 3)
        {
          S = -1;
        }
        else
        {
          S = 1;
        }
        if (N % 8 == 5)
        {
          S1 = -1;
        }
        else
        {
          S1 = 1;
        }
        DegreeAurif = Totient(N1) / 2;
        for (k = 1; k <= DegreeAurif; k += 2)
        {
          AurifQ[k] = JacobiSymbol(N, k);
        }
        for (k = 2; k <= DegreeAurif; k += 2)
        {
          t1 = k; // Calculate t2 = gcd(k, N1)
          t2 = N1;
          while (t1 != 0)
          {
            t3 = t2 % t1;
            t2 = t1;
            t1 = t3;
          }
          AurifQ[k] = Moebius(N1 / t2) * Totient(t2) * Cos((N - 1) * k);
        }
        Gamma[0] = Delta[0] = 1;
        for (k = 1; k <= DegreeAurif / 2; k++)
        {
          Gamma[k] = Delta[k] = 0;
          for (j = 0; j < k; j++)
          {
            Gamma[k] =
              Gamma[k]
                + N * AurifQ[2 * k
                - 2 * j
                - 1] * Delta[j]
                - AurifQ[2 * k
                - 2 * j] * Gamma[j];
            Delta[k] =
              Delta[k]
                + AurifQ[2 * k
                + 1
                - 2 * j] * Gamma[j]
                - AurifQ[2 * k
                - 2 * j] * Delta[j];
          }
          Gamma[k] /= 2 * k;
          Delta[k] = (Delta[k] + Gamma[k]) / (2 * k + 1);
        }
        for (k = DegreeAurif / 2 + 1; k <= DegreeAurif; k++)
        {
          Gamma[k] = Gamma[DegreeAurif - k];
        }
        for (k = (DegreeAurif + 1) / 2; k < DegreeAurif; k++)
        {
          Delta[k] = Delta[DegreeAurif - k - 1];
        }
        q = Expon / Base;
        L = 1;
        while (L * L <= q)
        {
          if (q % L == 0)
          {
            GetAurifeuilleFactor(L, BigBase);
            if (q != L * L)
            {
              GetAurifeuilleFactor(q / L, BigBase);
            }
          }
          L += 2;
        }
      }
    }
  }
  // Sort the factors
  void SortFactors()
  {
    int j, k;
    BigInteger Nro1;
    for (k = 0; k < NroFact - 1; k++)
    {
      for (j = k + 1; j < NroFact; j++)
      {
        if (Factores[k].compareTo(Factores[j]) > 0)
        {
          Nro1 = Factores[k];
          Factores[k] = Factores[j];
          Factores[j] = Nro1;
        }
      }
    }
    for (k = 0; k < NroFact; k++)
    {
      PD[NbrFactors + k - 1] = Factores[k];
      Exp[NbrFactors + k - 1] = 1;
      Typ[NbrFactors + k - 1] = -1; /* Unknown */
    }
    NbrFactors += k - 1;
  }

  int JacobiSymbol(int M, int Q)
  {
    int k, t1, t2, t3, jacobi;
    if (onlyFactoring)
    {

      // Calculate gcd(M,Q)

      t1 = M;
      t2 = Q;
      while (t1 != 0)
      {
        t3 = t2 % t1;
        t2 = t1;
        t1 = t3;
      }
      if (t2 > 1)
      {
        return 0;
      }
      jacobi = 1;
      while (Q % 2 == 0)
      {
        Q /= 2;
      }
      if (Q % 3 == 0)
      {
        do
        {
          jacobi = (jacobi * M) % 3;
          Q /= 3;
        }
        while (Q % 3 == 0);
        jacobi = (jacobi + 1) % 3 - 1;
      }

      k = 5;
      while (k * k <= Q)
      {
        if (k % 3 != 0)
        {
          while (Q % k == 0)
          {
            Q /= k;
            jacobi = (jacobi + k) % k;
            for (t1 = (k - 1) / 2; t1 > 0; t1--)
            {
              jacobi = jacobi * M % k;
            }
            jacobi = (jacobi + 1) % k - 1;
          }
        }
        k += 2;
      }
      if (Q > 1)
      {
        jacobi = (jacobi + Q) % Q;
        for (t1 = (Q - 1) / 2; t1 > 0; t1--)
        {
          jacobi = jacobi * M % Q;
        }
        jacobi = (jacobi + 1) % Q - 1;
      }
      return jacobi;
    }
    else
    {
      return 0;
    }
  }

  static int Cos(int N)
  {
    switch (N % 8)
    {
      case 0 :
        return 1;
      case 4 :
        return -1;
    }
    return 0;
  }

  int Totient(int N)
  {
    int totient, q, k;

    if (onlyFactoring)
    {
      totient = q = N;
      if (q % 2 == 0)
      {
        totient /= 2;
        do
        {
          q /= 2;
        }
        while (q % 2 == 0);
      }
      if (q % 3 == 0)
      {
        totient = totient * 2 / 3;
        do
        {
          q /= 3;
        }
        while (q % 3 == 0);
      }
      k = 5;
      while (k * k <= q)
      {
        if (k % 3 != 0)
        {
          if (q % k == 0)
          {
            totient = totient * (k - 1) / k;
            do
            {
              q /= k;
            }
            while (q % k == 0);
          }
        }
        k += 2;
      }
      if (q > 1)
      {
        totient = totient * (q - 1) / q;
      }
      return totient;
    }
    else
    {
      return 0;
    }
  }

  int Moebius(int N)
  {
    int moebius, q, k;

    if (onlyFactoring)
    {
      moebius = 1;
      q = N;
      if (q % 2 == 0)
      {
        moebius = -moebius;
        q /= 2;
        if (q % 2 == 0)
        {
          return 0;
        }
      }
      if (q % 3 == 0)
      {
        moebius = -moebius;
        q /= 3;
        if (q % 3 == 0)
        {
          return 0;
        }
      }
      k = 5;
      while (k * k <= q)
      {
        if (k % 3 != 0)
        {
          while (q % k == 0)
          {
            moebius = -moebius;
            q /= k;
            if (q % k == 0)
            {
              return 0;
            }
          }
        }
        k += 2;
      }
      if (q > 1)
      {
        moebius = -moebius;
      }
      return moebius;
    }
    else
    {
      return 0;
    }
  }

  void InsertFactor(BigInteger N)
  {
    int g;

    for (g = NroFact - 1; g >= 0; g--)
    {
      Factores[NroFact] = Factores[g].gcd(N);
      if (Factores[NroFact].equals(BigInt1) == false
        && Factores[NroFact].equals(Factores[g]) == false)
      {
        Factores[g] = Factores[g].divide(Factores[NroFact]);
        NroFact++;
      }
    }
  }

  void GetAurifeuilleFactor(int L, BigInteger BigBase)
  {
    BigInteger X, Csal, Dsal, Nro1;
    int k;

    if (onlyFactoring)
    {
      X = BigBase.pow(L);
      Csal = Dsal = BigInt1;
      for (k = 1; k < DegreeAurif; k++)
      {
        Csal = Csal.multiply(X).add(BigInteger.valueOf(Gamma[k]));
        Dsal = Dsal.multiply(X).add(BigInteger.valueOf(Delta[k]));
      }
      Csal = Csal.multiply(X).add(BigInteger.valueOf(Gamma[k]));
      Nro1 = Dsal.multiply(BigBase.pow((L + 1) / 2));
      InsertFactor(Csal.add(Nro1));
      InsertFactor(Csal.subtract(Nro1));
    }
  }

  boolean ComputeFourSquares(BigInteger PD[], int Exp[])
  {
    if (onlyFactoring)
    {
      int indexPrimes;
      BigInteger p, q, K, Mult1, Mult2, Mult3, Mult4;
      BigInteger Tmp, Tmp1, Tmp2, Tmp3, M1, M2, M3, M4;

      Quad1 = BigInt1; /* 1 = 1^2 + 0^2 + 0^2 + 0^2 */
      Quad2 = BigInt0;
      Quad3 = BigInt0;
      Quad4 = BigInt0;
      for (indexPrimes = NbrFactors - 1; indexPrimes >= 0; indexPrimes--)
      {
        if (Exp[indexPrimes] % 2 == 0)
        {
          continue;
        }
        p = PD[indexPrimes];
        q = p.subtract(BigInt1); /* q = p-1 */
        if (p.equals(BigInt2))
        {
          Mult1 = BigInt1; /* 2 = 1^2 + 1^2 + 0^2 + 0^2 */
          Mult2 = BigInt1;
          Mult3 = BigInt0;
          Mult4 = BigInt0;
        }
        else
        { /* Prime not 2 */
          if (p.testBit(1) == false)
          { /* if p = 1 (mod 4) */
            K = BigInt1;
            do
            { // Compute Mult1 = sqrt(-1) mod p
              K = K.add(BigInt1);
              Mult1 = K.modPow(q.shiftRight(2), p);
            }
            while (Mult1.equals(BigInt1) || Mult1.equals(q));
            if (Mult1.multiply(Mult1).mod(p).equals(q) == false)
            {
              return false; /* The number is not prime */
            }
            Mult2 = BigInt1;
            while (true)
            {
              K = Mult1.multiply(Mult1).add(Mult2.multiply(Mult2)).divide(p);
              if (K.equals(BigInt1))
              {
                Mult3 = BigInt0;
                Mult4 = BigInt0;
                break;
              }
              if (p.mod(K).signum() == 0)
              {
                return false; /* The number is not prime */
              }
              M1 = Mult1.mod(K);
              M2 = Mult2.mod(K);
              if (M1.compareTo(K.shiftRight(1)) > 0)
              {
                M1 = M1.subtract(K);
              }
              if (M2.compareTo(K.shiftRight(1)) > 0)
              {
                M2 = M2.subtract(K);
              }
              Tmp = Mult1.multiply(M1).add(Mult2.multiply(M2)).divide(K);
              Mult2 = Mult1.multiply(M2).subtract(Mult2.multiply(M1)).divide(K);
              Mult1 = Tmp;
            } /* end while */
          } /* end p = 1 (mod 4) */
          else
          { /* if p = 3 (mod 4) */
            // Compute Mult1 and Mult2 so Mult1^2 + Mult2^2 = -1 (mod p)
            Mult1 = BigInt0;
            do
            {
              Mult1 = Mult1.add(BigInt1);
            }
            while (BigInt1
              .negate()
              .subtract(Mult1.multiply(Mult1))
              .modPow(q.shiftRight(1), p)
              .compareTo(BigInt1)
              > 0);
            Mult2 =
              BigInt1.negate().subtract(Mult1.multiply(Mult1)).modPow(
                p.add(BigInt1).shiftRight(2),
                p);
            Mult3 = BigInt1;
            Mult4 = BigInt0;
            while (true)
            {
              K =
                Mult1
                  .multiply(Mult1)
                  .add(Mult2.multiply(Mult2))
                  .add(Mult3.multiply(Mult3))
                  .add(Mult4.multiply(Mult4))
                  .divide(p);
              if (K.equals(BigInt1))
              {
                break;
              }
              if (K.testBit(0) == false)
              { // If K is even ...
                if (Mult1.add(Mult2).testBit(0))
                {
                  if (Mult1.add(Mult3).testBit(0) == false)
                  {
                    Tmp = Mult2;
                    Mult2 = Mult3;
                    Mult3 = Tmp;
                  }
                  else
                  {
                    Tmp = Mult2;
                    Mult2 = Mult4;
                    Mult4 = Tmp;
                  }
                } // At this moment Mult1+Mult2 = even, Mult3+Mult4 = even
                Tmp1 = Mult1.add(Mult2).shiftRight(1);
                Tmp2 = Mult1.subtract(Mult2).shiftRight(1);
                Tmp3 = Mult3.add(Mult4).shiftRight(1);
                Mult4 = Mult3.subtract(Mult4).shiftRight(1);
                Mult3 = Tmp3;
                Mult2 = Tmp2;
                Mult1 = Tmp1;
                continue;
              } /* end if k is even */
              M1 = Mult1.mod(K);
              M2 = Mult2.mod(K);
              M3 = Mult3.mod(K);
              M4 = Mult4.mod(K);
              if (M1.compareTo(K.shiftRight(1)) > 0)
              {
                M1 = M1.subtract(K);
              }
              if (M2.compareTo(K.shiftRight(1)) > 0)
              {
                M2 = M2.subtract(K);
              }
              if (M3.compareTo(K.shiftRight(1)) > 0)
              {
                M3 = M3.subtract(K);
              }
              if (M4.compareTo(K.shiftRight(1)) > 0)
              {
                M4 = M4.subtract(K);
              }
              Tmp1 =
                Mult1
                  .multiply(M1)
                  .add(Mult2.multiply(M2))
                  .add(Mult3.multiply(M3))
                  .add(Mult4.multiply(M4))
                  .divide(K);
              Tmp2 =
                Mult1
                  .multiply(M2)
                  .subtract(Mult2.multiply(M1))
                  .add(Mult3.multiply(M4))
                  .subtract(Mult4.multiply(M3))
                  .divide(K);
              Tmp3 =
                Mult1
                  .multiply(M3)
                  .subtract(Mult3.multiply(M1))
                  .subtract(Mult2.multiply(M4))
                  .add(Mult4.multiply(M2))
                  .divide(K);
              Mult4 =
                Mult1
                  .multiply(M4)
                  .subtract(Mult4.multiply(M1))
                  .add(Mult2.multiply(M3))
                  .subtract(Mult3.multiply(M2))
                  .divide(K);
              Mult3 = Tmp3;
              Mult2 = Tmp2;
              Mult1 = Tmp1;
            } /* end while */
          } /* end if p = 3 (mod 4) */
        } /* end prime not 2 */
        Tmp1 =
          Mult1.multiply(Quad1).add(Mult2.multiply(Quad2)).add(
            Mult3.multiply(Quad3)).add(
            Mult4.multiply(Quad4));
        Tmp2 =
          Mult1
            .multiply(Quad2)
            .subtract(Mult2.multiply(Quad1))
            .add(Mult3.multiply(Quad4))
            .subtract(Mult4.multiply(Quad3));
        Tmp3 =
          Mult1
            .multiply(Quad3)
            .subtract(Mult3.multiply(Quad1))
            .subtract(Mult2.multiply(Quad4))
            .add(Mult4.multiply(Quad2));
        Quad4 =
          Mult1
            .multiply(Quad4)
            .subtract(Mult4.multiply(Quad1))
            .add(Mult2.multiply(Quad3))
            .subtract(Mult3.multiply(Quad2));
        Quad3 = Tmp3;
        Quad2 = Tmp2;
        Quad1 = Tmp1;
      } /* end for indexPrimes */
      for (indexPrimes = 0; indexPrimes < NbrFactors; indexPrimes++)
      {
        p = PD[indexPrimes].pow(Exp[indexPrimes] / 2);
        Quad1 = Quad1.multiply(p);
        Quad2 = Quad2.multiply(p);
        Quad3 = Quad3.multiply(p);
        Quad4 = Quad4.multiply(p);
      }
      Quad1 = Quad1.abs();
      Quad2 = Quad2.abs();
      Quad3 = Quad3.abs();
      Quad4 = Quad4.abs();
      // Sort squares
      if (Quad1.compareTo(Quad2) < 0)
      {
        Tmp = Quad1;
        Quad1 = Quad2;
        Quad2 = Tmp;
      }
      if (Quad1.compareTo(Quad3) < 0)
      {
        Tmp = Quad1;
        Quad1 = Quad3;
        Quad3 = Tmp;
      }
      if (Quad1.compareTo(Quad4) < 0)
      {
        Tmp = Quad1;
        Quad1 = Quad4;
        Quad4 = Tmp;
      }
      if (Quad2.compareTo(Quad3) < 0)
      {
        Tmp = Quad2;
        Quad2 = Quad3;
        Quad3 = Tmp;
      }
      if (Quad2.compareTo(Quad4) < 0)
      {
        Tmp = Quad2;
        Quad2 = Quad4;
        Quad4 = Tmp;
      }
      if (Quad3.compareTo(Quad4) < 0)
      {
        Tmp = Quad3;
        Quad3 = Quad4;
        Quad4 = Tmp;
      }
    }
    return true;
  }

  BigInteger FactoringSIQS(BigInteger NbrToFactor)
  {
    BigInteger RealNbrToFactor = null;
    long FactorBase;
    int SieveLimit;
    int Expon;
    int NbrPrimes, NbrPrimes2;
    int s, F, F1, F2, F3, F4, X1, X2;
    long currentPrime;
    int NbrMod;
    long modsqrt[];
    long prime[];
    byte logar[];
    long ainv[];
    int Bainv2[][];
    int soln1[];
    int difsoln[];
    long afact[];
    int aindex[];
    int amodq[];
    byte SieveArray[];
    long Power, Power2, Square, SqrRootMod, fact;
    int index;
    int index2;
    int PolynomialIndex;
    long D, E, Q, V, W, X, Y, Z, T1, V1, W1, Y1;
    long t1, t2, t3;
    double Temp, Prod;
    byte threshold, logprime;
    int NbrPolynomials;
    int PolynomialSetNbr = 0;
    int pp = 0;
    double bestadjust;
    int i, j, k, multiplier, r;
    int arrmult[] = { 1, 2, 3, 5, 7, 11, 13, 17, 19, 23 };
    double adjustment[] = new double[arrmult.length];
    long seed = 0;
    int span, min;
    int mask;
    int vectExpParity[];
    int matrixV[];
    int matrixB[][];
    int rowMatrixB[];
    long vectLeftHandSide[][];
    int nbrColumns, expParity;
    int matrixPartial[][];
    int matrixPartialHashIndex[];
    int prev;
    int nbrPartials = 0;
    int smallPrimeLowerLimit, smallPrimeUpperLimit;
    int firstLimit;
    int secondLimit;
    int thirdLimit;
    double dNumberToFactor;
    String SIQSInfoText;
    long Divid, Divisor, Rem;
    long startTime;
    long biR0 = 0, biR1 = 0, biR2 = 0, biR3 = 0, biR4 = 0, biR5 = 0;
    long biR6 = 0, biR7 = 0, biR8 = 0, biR9 = 0, biR10 = 0;
    boolean cond = false;
    int S1, S2, G0, G1, G2, G3;
    boolean polyadd;
    ValuesSieved = 0;

    Temp = Math.log(NbrToFactor.doubleValue());
    NbrPrimes = (int) Math.exp(Math.sqrt(Temp * Math.log(Temp)) * 0.318);
    SieveLimit = (int) Math.exp(8.5 + 0.015 * Temp);
    if (SieveLimit > 30000)
    {
      SieveLimit = 30000;
    }
    s = NbrToFactor.bitLength() / 28 + 1;
    prime = new long[NbrPrimes + 3];
    modsqrt = new long[NbrPrimes + 3];
    logar = new byte[NbrPrimes + 3];
    ainv = new long[NbrPrimes + 3];
    Bainv2 = new int[s][NbrPrimes + 3];
    soln1 = new int[NbrPrimes + 3];
    difsoln = new int[NbrPrimes + 3];
    afact = new long[s];
    aindex = new int[s];
    amodq = new int[s];
    rowMatrixB = new int[200];
    NbrPolynomials = (1 << (s - 1)) - 1;
    BigNbrToBigInt(NbrToFactor);
    TestNbr[NumberLength] = 0;
    for (i = NumberLength; i >= 0; i--)
    {
      TestNbr2[i] = TestNbr[i];
    }
    NumberLength++;
    matrixPartialHashIndex = new int[1024];
    for (i = matrixPartialHashIndex.length - 1; i >= 0; i--)
    {
      matrixPartialHashIndex[i] = -1;
    }
    textAreaContents = "";
    StringToLabel = "Factoring ";
    insertBigNbr(NbrToFactor);
    addStringToLabel("(" + NbrToFactor.toString().length() + " digits)");
    SIQSInfoText =
      textAreaContents
        + StringToLabel
        + "\n\nSIQS parameters: "
        + NbrPrimes
        + " primes, sieve limit: "
        + SieveLimit;
    lowerTextArea.setText(
      SIQSInfoText + "\nSearching for Knuth-Schroeppel multiplier...");

    /************************/
    /* Compute startup data */
    /************************/

    /* search for best Knuth-Schroeppel multiplier */
    bestadjust = -10.0e0;
    prime[0] = 1;
    prime[1] = 2;
    NbrMod = NbrToFactor.and(BigInteger.valueOf(7)).intValue();
    for (j=0; j<arrmult.length; j++)
    {
      int mod = NbrMod * arrmult[j] % 8;
      adjustment[j] = 0.34657359; /*  (ln 2)/2  */
      if (mod == 1)
        adjustment[j] *= (4.0e0);
      if (mod == 5)
        adjustment[j] *= (2.0e0);
      adjustment[j] -= Math.log((double) arrmult[j]) / (2.0e0);
    }
    currentPrime = 3;
    while (currentPrime < 10000)
    { 
      NbrMod = (int) RemDivBigNbrByLong(TestNbr, currentPrime);
      int jacobi = (int)modPow(NbrMod, (currentPrime - 1) / 2, currentPrime);
      double dp = (double) currentPrime;
      double logp = Math.log(dp) / dp;
      for (j=0; j<arrmult.length; j++)
      {
        if (arrmult[j] == currentPrime)
        {
          adjustment[j] += logp;
        }
        else if (jacobi * (int)modPow(arrmult[j], (currentPrime - 1) / 2,
                 currentPrime)%currentPrime == 1)
        {
          adjustment[j] += 2 * logp;
        }
      }
     calculate_new_prime1 :
      do
      {
        currentPrime += 2;
        for (Q = 3; Q * Q <= currentPrime; Q += 2)
        { /* Check if currentPrime is prime */
          if (currentPrime % Q == 0)
          {
            continue calculate_new_prime1;
          }
        }
        break; /* Prime found */
      }
      while (true);
    }  /* end while */
    multiplier = 1;
    for (j=0; j<arrmult.length; j++)
    {
      if (adjustment[j] > bestadjust)
      { /* find biggest adjustment */
        bestadjust = adjustment[j];
        multiplier = arrmult[j];
      }
    } /* end while */
    MultBigNbrByLong(TestNbr2, multiplier, TestNbr);
    if (TestNbr[NumberLength - 1] != 0 || TestNbr[NumberLength - 2] > Mi)
    {
      TestNbr[NumberLength] = 0;
      NumberLength++;
    }
    matrixPartial = new int[NbrPrimes * 8][NumberLength + 2];
    dN = (double) TestNbr[NumberLength - 2];
    if (NumberLength > 1)
    {
      dN += (double) TestNbr[NumberLength - 3] / dDosALa31;
    }
    if (NumberLength > 2)
    {
      dN += (double) TestNbr[NumberLength - 4] / dDosALa62;
    }
    FactorBase = currentPrime;
    matrixB = new int[(int) ((float) NbrPrimes * 1.05 + 40)][];
    vectLeftHandSide = new long[matrixB.length][];
    vectExpParity = new int[matrixB.length];
    modsqrt[1] = NbrToFactor.testBit(0) ? 1 : 0;
    switch ((int)TestNbr[0] & 0x07)
    {
      case 1:
        logar[1] = 4;
        break;
      case 5:
        logar[1] = 2;
        break;
      default:
        logar[1] = 1;
        break;
    }
    if (multiplier != 1)
    {
      prime[2] = multiplier;
      logar[2] =
          (byte) (Math.round(Math.log((double) multiplier) / Math.log(2)));
      modsqrt[2] = 0;
      j = 3;
    }
    else
    {
      j = 2;
    }
    currentPrime = 3;
    while (j < NbrPrimes)
    { /* select small primes */
      NbrMod = (int) RemDivBigNbrByLong(TestNbr, currentPrime);
      if (modPow(NbrMod, (currentPrime - 1) / 2, currentPrime) == 1)
      {
        /* use only if Jacobi symbol = 0 or 1 */
        prime[j] = currentPrime;
        NbrMod = (int) RemDivBigNbrByLong(TestNbr, currentPrime);
        if (currentPrime % 4 == 3)
        {
          SqrRootMod = modPow(NbrMod, (currentPrime + 1) / 4, currentPrime);
        }
        else
        {
          if (currentPrime % 8 == 5)
          {
            SqrRootMod =
              modPow(NbrMod * 2, (currentPrime - 5) / 8, currentPrime);
            SqrRootMod =
              ((((2 * NbrMod * SqrRootMod % currentPrime) * SqrRootMod - 1)
                % currentPrime)
                * NbrMod
                % currentPrime)
                * SqrRootMod
                % currentPrime;
          }
          else
          { /* p = 1 (mod 8) */
            Q = currentPrime - 1;
            E = 0;
            Power2 = 1;
            do
            {
              E++;
              Q /= 2;
              Power2 *= 2;
            }
            while ((Q & 1) == 0); /* E >= 3 */
            Power2 /= 2;
            X = 1;
            do
            {
              X++;
              Z = modPow(X, Q, currentPrime);
            }
            while (modPow(Z, Power2, currentPrime) == 1);
            Y = Z;
            X = modPow(NbrMod, (Q - 1) / 2, currentPrime);
            V = NbrMod * X % currentPrime;
            W = V * X % currentPrime;
            while (W != 1)
            {
              T1 = 0;
              D = W;
              while (D != 1)
              {
                D = D * D % currentPrime;
                T1++;
              }
              D = modPow(Y, 1 << (E - T1 - 1), currentPrime);
              Y1 = D * D % currentPrime;
              E = T1;
              V1 = V * D % currentPrime;
              W1 = W * Y1 % currentPrime;
              Y = Y1;
              V = V1;
              W = W1;
            } /* end while */
            SqrRootMod = V;
          } /* end if */
        } /* end if */
        modsqrt[j] = SqrRootMod;
        logar[j] =
          (byte) (Math.round(Math.log((double) currentPrime) / Math.log(2)));
        j++;
      } /* end while */
      calculate_new_prime2 : do
      {
        currentPrime += 2;
        for (Q = 3; Q * Q <= currentPrime; Q += 2)
        { /* Check if currentPrime is prime */
          if (currentPrime % Q == 0)
          {
            continue calculate_new_prime2;
          }
        }
        break; /* Prime found */
      }
      while (true);
    } /* End while */

    FactorBase = currentPrime;
    SieveArray =
      new byte[2 * SieveLimit + 1 > (int) FactorBase
        ? 2 * SieveLimit + 5000
        : (int) FactorBase + 5000];
    dNumberToFactor = NbrToFactor.doubleValue();
    SIQSInfoText += "\nMultiplier: " + multiplier + ", factor base: " + FactorBase;
    lowerTextArea.setText(SIQSInfoText);
    firstLimit = 2;
    for (j = 2; j < NbrPrimes; j++)
    {
      firstLimit *= (int) prime[j];
      if (firstLimit > 2 * SieveLimit)
      {
        break;
      }
    }
    smallPrimeLowerLimit = j;
    smallPrimeUpperLimit = j + 1;
    int logarsmall = logar[j + 1];
    threshold =
      (byte) (Math
        .log(
          Math.sqrt(dNumberToFactor) * SieveLimit / (FactorBase * 64) / prime[j
            + 1])
        / Math.log(2)+1);
    firstLimit = (int) (Math.log(dNumberToFactor) / 3);
    for (secondLimit = firstLimit; secondLimit < NbrPrimes; secondLimit++)
    {
      if (prime[secondLimit] * 2 > SieveLimit)
      {
        break;
      }
    }
    for (thirdLimit = secondLimit; thirdLimit < NbrPrimes; thirdLimit++)
    {
      if (prime[thirdLimit] > 2 * SieveLimit)
      {
        break;
      }
    }
    NbrPrimes2 = NbrPrimes - 4;
    startTime = System.currentTimeMillis();
    // Sieve start time in milliseconds.
    sieve_stage : do
    {
      /*********************************************/
      /* Initialization stage for first polynomial */
      /*********************************************/
      PolynomialIndex = 1;
      Prod = Math.sqrt(2 * dNumberToFactor) / (double) SieveLimit;
      fact = (long) Math.pow(Prod, 1 / (float) s);
      for (i = 2;; i++)
      {
        if (prime[i] > fact)
        {
          break;
        }
      }
      span = (int) (NbrPrimes / s / s / 2);
      if (NbrPrimes < 500)
      {
        span *= 2;
      }
      min = i - span / 2;
      for (index = 0; index < s; index++)
      {
        do
        {
          seed = (1141592621 * seed + 321435) & 0xFFFFFFFFl;
          i = (int) (((seed * span) >> 32) + min);
          for (index2 = 0; index2 < index; index2++)
          {
            if (aindex[index2] == i || aindex[index2] == i + 1)
            {
              break;
            }
          }
        }
        while (index2 < index);
        afact[index] = prime[i];
        aindex[index] = i;
      }

      // Compute the leading coefficient in biS.

      LongToBigNbr(afact[0], biS);
      for (index = 1; index < s; index++)
      {
        MultBigNbrByLong(biS, afact[index], biS);
      }
      for (index = 0; index < s; index++)
      {
        D = 1;
        E = afact[index];
        for (index2 = 0; index2 < s; index2++)
        {
          if (index != index2)
          {
            D = D * afact[index2] % E;
          }
        }
        amodq[index] = (int) D;
        Q = modsqrt[aindex[index]] * modInv(D, E) % E;
        if (Q > E / 2)
        {
          Q = E - Q;
        }
        DivBigNbrByLong(biS, E, biR);
        MultBigNbrByLong(biR, Q, aiJS[index]);
      }
      for (index = 0; index < NumberLength; index++)
      {
        biN[index] = aiJS[0][index];
      }
      for (index2 = 1; index2 < s; index2++)
      {
        AddBigNbr(biN, aiJS[index2], biN);
      }
      for (index = 1; index < NbrPrimes; index++)
      {
        D = 1;
        E = prime[index];
        for (index2 = 0; index2 < s; index2++)
        {
          D = D * afact[index2] % E;
        }
        ainv[index] = modInv(D, E);
        for (index2 = 0; index2 < s; index2++)
        {
          Bainv2[index2][index] =
            (int) (RemDivBigNbrByLong(aiJS[index2], E) * 2 * ainv[index] % E);
        }
        D = RemDivBigNbrByLong(biN, E);
        soln1[index] =
          (int) ((SieveLimit + ainv[index] * (E + modsqrt[index] - D)) % E + 100*E);
        difsoln[index] =
          (int) ((2*ainv[index] * (E - modsqrt[index])) % E);
      }
      do
      {
        if (onlyFactoring)
        {
          polynomialsSieved++;
        }
        /***************/
        /* Sieve stage */
        /***************/
        D = PolynomialIndex;
        index2 = 0;
        while ((D & 1) == 0)
        {
          D /= 2;
          index2++;
        }
        if (polyadd = ((D & 2) != 0))
        {
          AddBigNbr(biN, aiJS[index2], biN);
          AddBigNbr(biN, aiJS[index2], biN);
        }
        else
        {
          SubtractBigNbr(biN, aiJS[index2], biN);
          SubtractBigNbr(biN, aiJS[index2], biN);
        }
        int[] rowBainv2 = Bainv2[index2];
        // Compute solutions for divisors of the leading coefficients
        for (index2 = 0; index2 < s; index2++)
        {
          index = aindex[index2];
          E = prime[index];
          D = RemDivBigNbrByLong(TestNbr, E * E);
          Q = RemDivBigNbrByLong(biN, E * E);
          soln1[index] =
            (int) (((D - Q * Q) / E * modInv(amodq[index2] * Q % E, E) % E
              + E
              + SieveLimit)
              % E + 100*E);
          difsoln[index] = -1;   // Only one solution.
        }
        X1 = 2 * SieveLimit;
        F1 = polyadd ? -rowBainv2[1] : rowBainv2[1];
        if ((soln1[1] += F1) % 2 == 0)
        {
          SieveArray[0] = (byte) (logar[1] - threshold);
          SieveArray[1] = (byte) (-threshold);
        }
        else
        {
          SieveArray[0] = (byte) (-threshold);
          SieveArray[1] = (byte) (logar[1] - threshold);
        }
        F2 = 2;
        index = 2;
        while (true)
        {
          F = (int) prime[index];
          F3 = F2 * F;
          if (X1 + 1 < F3)
          {
            F3 = X1 + 1;
          }
          F4 = F2;
          while (F4 * 2 <= F3)
          {
            System.arraycopy(SieveArray, 0, SieveArray, F4, F4);
            F4 *= 2;
          }
          System.arraycopy(SieveArray, 0, SieveArray, F4, F3 - F4);
          if (F3 == X1 + 1)
          {
            break;
          }
          logprime = logar[index];
          F1 = polyadd ? -rowBainv2[index] : rowBainv2[index];
          for (index2 = (soln1[index] += F1) % F; index2 < F3; index2 += F)
          {
            SieveArray[index2] += logprime;
          }
          if (F != multiplier)
          {
            for (index2 = (soln1[index] + difsoln[index]) % F; index2 < F3; index2 += F)
            {
              SieveArray[index2] += logprime;
            }
          }
          index++;
          F2 *= F;
        }

        for (; index < smallPrimeUpperLimit; index++)
        {
          F = (int) prime[index];
          F1 = polyadd ? -rowBainv2[index] : rowBainv2[index];
          soln1[index] += F1;
        }

        int primesmall = (int) prime[index - 1];
        int soln1small = soln1[index - 1];
        int soln2small = (soln1small + difsoln[index - 1]) % primesmall;
        for (; index < firstLimit; index++)
        {
          F = (int) prime[index];
          F1 = polyadd ? -rowBainv2[index] : rowBainv2[index];
          F2 = F + F;
          F3 = F2 + F;
          F4 = F3 + F;
          S1 = (soln1[index] += F1) % F;
          S2 = (S1 + difsoln[index]) % F;
          if ((G0 = S2 - S1) < 0)
          {
            S1 = S2;
            G0 = -G0;
          }
          G1 = G0 + F;
          G2 = G1 + F;
          G3 = G2 + F;
          logprime = logar[index];
          index2 = X1 / F4 * F4 + S1;
          do
          {
            SieveArray[index2] += logprime;
            SieveArray[index2 + F] += logprime;
            SieveArray[index2 + F2] += logprime;
            SieveArray[index2 + F3] += logprime;
            SieveArray[index2 + G0] += logprime;
            SieveArray[index2 + G1] += logprime;
            SieveArray[index2 + G2] += logprime;
            SieveArray[index2 + G3] += logprime;
          }
          while ((index2 -= F4) >= 0);
        }
        for (; index < secondLimit; index++)
        {
          F = (int) prime[index];
          F1 = (polyadd ? -rowBainv2[index] : rowBainv2[index]);
          X2 = X1 - 4 * F;
          F2 = F + F;
          F3 = F2 + F;
          F4 = F2 + F2;
          logprime = logar[index];
          for (index2 = (soln1[index] += F1) % F; index2 <= X2; index2 += F4)
          {
            SieveArray[index2] += logprime;
            SieveArray[index2 + F] += logprime;
            SieveArray[index2 + F2] += logprime;
            SieveArray[index2 + F3] += logprime;
          }
          for (; index2 <= X1; index2 += F)
          {
            SieveArray[index2] += logprime;
          }
          if (difsoln[index] >= 0)
          {
            for (index2 = (index2 + difsoln[index]) % F; index2 <= X2; index2 += F4)
            {
              SieveArray[index2] += logprime;
              SieveArray[index2 + F] += logprime;
              SieveArray[index2 + F2] += logprime;
              SieveArray[index2 + F3] += logprime;
            }
            for (; index2 <= X1; index2 += F)
            {
              SieveArray[index2] += logprime;
            }
          }
        }
        for (; index < thirdLimit; index++)
        {
          F = (int) prime[index];
          logprime = logar[index];
          F1 = (polyadd ? -rowBainv2[index] : rowBainv2[index]);
          for (index2 = (soln1[index] += F1) % F; index2 <= X1; index2 += F)
          {
            SieveArray[index2] += logprime;
          }
          for (index2 = (index2 + difsoln[index]) % F; index2 <= X1; index2 += F)
          {
            SieveArray[index2] += logprime;
          }
        }
        if (polyadd)
        {
          for (; index < NbrPrimes2; index++)
          {
            logprime = logar[index];
            F = (int) prime[index];
            F1 = -rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = -rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = -rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = -rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
          }
          for (; index < NbrPrimes; index++)
          {
            logprime = logar[index];
            F = (int) prime[index];
            F1 = -rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2+difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
          }
        }
        else
        {
          for (; index < NbrPrimes2; index++)
          {
            logprime = logar[index];
            F = (int) prime[index];
            F1 = rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            F = (int) prime[++index];
            F1 = rowBainv2[index];
            if ((F2 = (soln1[index] += F1) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
          }
          for (; index < NbrPrimes; index++)
          {
            logprime = logar[index];
            F = (int) prime[index];
            if ((F2 = (soln1[index] += rowBainv2[index]) % F) < X1)
            {
              SieveArray[F2] += logprime;
            }
//            if ((F2 = (F2 + difsoln[index]) % F) < X1)
            if ((F2 += difsoln[index]) > F)
            {
              F2 -= F;
            }
            if (F2 < X1)
            {
              SieveArray[F2] += logprime;
            }
          }
        }
        ValuesSieved += 2*SieveLimit;
        /************************/
        /* Trial division stage */
        /************************/
        index2 = 2 * SieveLimit + 1;
        do
        {
          if (SieveArray[--index2] >= 0 && SieveArray[index2] < 64)
          {
            if (SieveArray[index2] < logarsmall)
            {
              if ((index2 - soln1small) % primesmall != 0)
              {
                if ((index2 - soln2small) % primesmall != 0)
                {
                  continue;
                }
              }
            }
            trialDivisions++;
            MultBigNbrByLong(biS, index2 - SieveLimit, biT);
            AddBigNbr(biT, biN, biT);
            MultBigNbr(biT, biT, biR);
            SubtractBigNbr(biR, TestNbr, biR); // Number to factor: (Ax+B)^2-N
            for (i = 0; i < NumberLength; i++)
            {
              biT[i] = biR[i];
            }
            /* factor biR */

            if (multiplier > 1)
            {
              while (RemDivBigNbrByLong(biR, multiplier * multiplier) == 0)
              {
                DivBigNbrByLong(biR, multiplier * multiplier, biR);
              }
            }
            F = NumberLength; /* Back up NumberLength */
            boolean positive = true;
            if (biR[NumberLength - 1] >= 0x40000000)
            { /* Negative */
              positive = false;
              ChSignBigNbr(biR);    // Convert to positive.
            }
            for (index = 0; index < s; index++)
            {
              DivBigNbrByLong(biR, afact[index], biR);
              if ((biR[NumberLength - 1] == 0
                && biR[NumberLength - 2] < 0x40000000l))
              {
                NumberLength--;
              }
            }
            switch (NumberLength)
            {
              case 7 :
                biR6 = biR[6];
              case 6 :
                biR5 = biR[5];
              case 5 :
                biR4 = biR[4];
              case 4 :
                biR3 = biR[3];
              case 3 :
                biR2 = biR[2];
              case 1 :
              case 2 :
                biR1 = biR[1];
                biR0 = biR[0];
            }
            nbrColumns = 0;
            if (NumberLength <= 2)
            {
              Divid = (biR1 << 31) | biR0;
              for (index = 1; index < NbrPrimes; index++)
              {
                Divisor = prime[index];
                while (Divid % Divisor == 0)
                {
                  Divid /= Divisor;
                }
              }
            }
            else
            {
              Divid = 0;
              for (index = 1; index < NbrPrimes; index++)
              {
                while (true)
                {
                  Divisor = prime[index];
                  Rem = 0;
                  switch (NumberLength)
                  {
                    case 7 :
                      Rem = (biR6 + (Rem << 31)) % Divisor;
                    case 6 :
                      Rem = (biR5 + (Rem << 31)) % Divisor;
                    case 5 :
                      Rem = (biR4 + (Rem << 31)) % Divisor;
                    case 4 :
                      Rem = (biR3 + (Rem << 31)) % Divisor;
                    case 3 :
                      Rem = (biR2 + (Rem << 31)) % Divisor;
                      Rem = (biR1 + (Rem << 31)) % Divisor;
                      Rem = (biR0 + (Rem << 31)) % Divisor;
                  }
                  if (Rem != 0)
                  {
                    break;
                  }
                  switch (NumberLength)
                  {
                    case 7 :
                      Divid = biR6 + (Rem << 31);
                      Rem = Divid % Divisor;
                      biR6 = Divid / Divisor;
                    case 6 :
                      Divid = biR5 + (Rem << 31);
                      Rem = Divid % Divisor;
                      biR5 = Divid / Divisor;
                    case 5 :
                      Divid = biR4 + (Rem << 31);
                      Rem = Divid % Divisor;
                      biR4 = Divid / Divisor;
                    case 4 :
                      Divid = biR3 + (Rem << 31);
                      Rem = Divid % Divisor;
                      biR3 = Divid / Divisor;
                    case 3 :
                      Divid = biR2 + (Rem << 31);
                      Rem = Divid % Divisor;
                      biR2 = Divid / Divisor;
                      Divid = biR1 + (Rem << 31);
                      biR1 = Divid / Divisor;
                      biR0 = (biR0 + ((Divid % Divisor) << 31)) / Divisor;
                  }
                  switch (NumberLength)
                  {
                    case 7 :
                      cond = (biR6 == 0 && biR5 < 0x40000000);
                      break;
                    case 6 :
                      cond = (biR5 == 0 && biR4 < 0x40000000);
                      break;
                    case 5 :
                      cond = (biR4 == 0 && biR3 < 0x40000000);
                      break;
                    case 4 :
                      cond = (biR3 == 0 && biR2 < 0x40000000);
                      break;
                    case 3 :
                      cond = (biR2 == 0 && biR1 < 0x40000000);
                      break;
                  }
                  if (cond)
                  {
                    NumberLength--;
                    if (NumberLength == 2)
                    {
                      Divid = (biR1 << 31) | biR0;
                      int sqrtDivid = (int)Math.floor(Math.sqrt(Divid));
                      for (; index < NbrPrimes; index++)
                      {
                        Divisor = prime[index];
                        while (Divid % Divisor == 0)
                        {
                          Divid /= Divisor;
                          sqrtDivid = (int)Math.floor(Math.sqrt(Divid));
                        }
                        if (Divisor > sqrtDivid)
                        {
                          index = NbrPrimes-1;
                          if (Divid <= prime[index])
                          {
                            Divid = 1;
                          }
                          break;
                        }
                      }
                      break;
                    }
                  }
                }             /* end while */
              }               /* end for */
            }
            F2 = NumberLength;
            NumberLength = F;
            if (F2 == 2 && Divid == 1)
            { // Smooth relation found.
              if (positive == false)
              {
                rowMatrixB[nbrColumns++] = 0; // Insert -1 as a factor.
              }
              /* factor biT (backup) storing in BiR the square part */
              LongToBigNbr(1, biR);
              while (RemDivBigNbrByLong(biT, 3 * 3) == 0)
              {
                DivBigNbrByLong(biT, 3 * 3, biT);
                if (RemDivBigNbrByLong(TestNbr, 3) == 0)
                {
                  DivBigNbrByLong(biU, 3, biU);
                }
                else
                {
                  MultBigNbrByLong(biR, 3, biR);
                }
              }
              for (index = 5; index < 100; index += 4)
              {
                while (RemDivBigNbrByLong(biT, index * index) == 0)
                {
                  DivBigNbrByLong(biT, index * index, biT);
                  if (RemDivBigNbrByLong(TestNbr, index) == 0)
                  {
                    DivBigNbrByLong(biU, index, biU);
                  }
                  else
                  {
                    MultBigNbrByLong(biR, index, biR);
                  }
                }
                index += 2;
                while (RemDivBigNbrByLong(biR, index * index) == 0)
                {
                  DivBigNbrByLong(biR, index * index, biR);
                  if (RemDivBigNbrByLong(TestNbr, index) == 0)
                  {
                    DivBigNbrByLong(biU, index, biU);
                  }
                  else
                  {
                    MultBigNbrByLong(biR, index, biR);
                  }
                }
              }
              MultBigNbrByLong(biS, index2 - SieveLimit, biU);
              AddBigNbr(biU, biN, biU);
              for (index = 1; index < NbrPrimes; index++)
              {
                expParity = 0;
                D = prime[index];
                while (RemDivBigNbrByLong(biT, D) == 0)
                {
                  DivBigNbrByLong(biT, D, biT);
                  expParity = 1 - expParity;
                  if (expParity == 0)
                  {
                    if (RemDivBigNbrByLong(TestNbr, D) == 0)
                    {
                      DivBigNbrByLong(biU, D, biU);
                    }
                    else
                    {
                      MultBigNbrByLong(biR, D, biR);
                    }
                  }
                }
                if (expParity != 0)
                {
                  rowMatrixB[nbrColumns++] = index;
                  expParity = 0;
                }
              }

              if (InsertNewRelation(biR,
                biT,
                biU,
                nbrColumns,
                matrixB,
                rowMatrixB,
                pp,
                vectLeftHandSide))
              {
                smoothsFound++;
                pp++;
                ShowSIQSStatus(pp, matrixB.length, startTime);
                if (pp == matrixB.length)
                {
                  i = EraseSingletons(matrixB, vectLeftHandSide, vectExpParity);
                  if (i != 0)
                  {
                    lowerTextArea.setText(
                      SIQSInfoText + "\n" + i + " singletons discarded");
                    pp -= i;
                  }
                  else
                  {
                    lowerTextArea.setText(
                      SIQSInfoText
                        + "\nSolving congruence matrix using Block Lanczos algorithm");
                    if (LinearAlgebraPhase(NbrPrimes,
                      matrixB,
                      prime,
                      biT,
                      biR,
                      biU,
                      vectExpParity,
                      vectLeftHandSide))
                    {
                      return BigIntToBigNbr(biT); /* Factor found */
                    }
                    else
                    {
                      lowerTextArea.setText(
                        SIQSInfoText
                          + "\nLinear dependences were found. Discarding 50 congruences...");
                      pp -= 50; /* Factor not found */
                    }
                  }
                }
              }
              continue; /* Continue sieving */
            }
            else
            {
              if (F2 == 2 && Divid < 64 * FactorBase)
              {
                // Partial relation found.
                totalPartials++;
                if (positive == false)
                {
                  rowMatrixB[nbrColumns++] = 0; // Insert -1 as a factor.
                }
                // Check if there is already another relation with the same
                // factor outside the prime base.
                // Calculate hash index
                i = matrixPartialHashIndex[(int) (Divid & 0x7FE) / 2];
                prev = -1;
                while (i >= 0)
                {
                  if ((int) Divid == matrixPartial[i][0])
                  {
                    // Match of partials.
                    for (index = 0; index < NumberLength; index++)
                    {
                      biV[index] = matrixPartial[i][index + 2];
                    }
                    MultBigNbr(biV, biV, biT);
                    SubtractBigNbr(biT, TestNbr, biT);

                    /* factor biT */

                    D = (long) matrixPartial[i][0];
                    E = RemDivBigNbrByLong(TestNbr, D);
                    t1 = D;
                    t2 = E;
                    while (t1 != 0)
                    {
                      t3 = t2 % t1;
                      t2 = t1;
                      t1 = t3;
                    } // t2 = GCD(D, E)
                    LongToBigNbr(D, biR);
                    if (D < 0)
                    {
                      AddBigNbr(biR, TestNbr, biR);
                    }
                    if (t2 < 0)
                    {
                      t2 = -t2;
                    }
                    if (t2 != 1)
                    {
                      DivBigNbrByLong(TestNbr, t2 < 0 ? -t2 : t2, biU);
                      AddBigNbrModN(biR, biU, biR);
                    }
                    if (multiplier > 1)
                    {
                      while (RemDivBigNbrByLong(biT, multiplier * multiplier) == 0)
                      {
                        DivBigNbrByLong(biT, multiplier * multiplier, biT);
                        NumberLength--;
                        MultBigNbrByLongModN(biR, multiplier, biR);
                        NumberLength++;
                        if (RemDivBigNbrByLong(TestNbr, multiplier) == 0)
                        {
                          DivBigNbrByLong(TestNbr, multiplier, biU);
                          AddBigNbrModN(biR, biU, biR);
                        }
                      }
                    }
                    nbrColumns = 0;
                    for (index = 1; index < NbrPrimes; index++)
                    {
                      D = prime[index];
                      expParity = 0;
                      while (RemDivBigNbrByLong(biT, D) == 0)
                      {
                        expParity = 1 - expParity;
                        DivBigNbrByLong(biT, D, biT);
                        if (expParity == 0)
                        {
                          NumberLength--;
                          MultBigNbrByLongModN(biR, D, biR);
                          NumberLength++;
                          if (RemDivBigNbrByLong(TestNbr, D) == 0)
                          {
                            DivBigNbrByLong(TestNbr, D, biU);
                            AddBigNbrModN(biR, biU, biR);
                          }
                        }
                      }
                      if (expParity != 0)
                      {
                        rowMatrixB[nbrColumns++] = index;
                      }
                    }

                    MultBigNbrByLong(biS, index2 - SieveLimit, biT);
                    AddBigNbr(biT, biN, biW);
                    MultBigNbr(biW, biW, biT);
                    SubtractBigNbr(biT, TestNbr, biT);

                    /* factor biT */

                    if (multiplier > 1)
                    {
                      while (RemDivBigNbrByLong(biT, multiplier * multiplier) == 0)
                      {
                        DivBigNbrByLong(biT, multiplier * multiplier, biT);
                        NumberLength--;
                        MultBigNbrByLongModN(biR, multiplier, biR);
                        NumberLength++;
                        if (RemDivBigNbrByLong(TestNbr, multiplier) == 0)
                        {
                          DivBigNbrByLong(TestNbr, multiplier, biU);
                          AddBigNbrModN(biR, biU, biR);
                        }
                      }
                    }
                    for (index = 1; index < NbrPrimes; index++)
                    {
                      expParity = 0;
                      D = prime[index];
                      while (RemDivBigNbrByLong(biT, D) == 0)
                      {
                        expParity = 1 - expParity;
                        DivBigNbrByLong(biT, D, biT);
                        if (expParity == 0)
                        {
                          NumberLength--;
                          MultBigNbrByLongModN(biR, D, biR);
                          NumberLength++;
                          if (RemDivBigNbrByLong(TestNbr, D) == 0)
                          {
                            DivBigNbrByLong(TestNbr, D, biU);
                            AddBigNbrModN(biR, biU, biR);
                          }
                        }
                      }
                      if (expParity != 0)
                      {
                        // Check if the index is already in the row.
                        for (j = 0; j < nbrColumns; j++)
                        {
                          if (index <= rowMatrixB[j])
                          {
                            break;
                          }
                        }
                        if (j < nbrColumns && index == rowMatrixB[j])
                        {
                          // Index already in row.
                          D = prime[rowMatrixB[j]];
                          NumberLength--;
                          MultBigNbrByLongModN(biR, D, biR);
                          NumberLength++;
                          if (RemDivBigNbrByLong(TestNbr, D) == 0)
                          {
                            DivBigNbrByLong(TestNbr, D, biU);
                            AddBigNbrModN(biR, biU, biR);
                          }
                          // Delete entry from row.
                          for (k = j + 1; k < nbrColumns; k++)
                          {
                            rowMatrixB[k - 1] = rowMatrixB[k];
                          }
                          nbrColumns--;
                        }
                        else
                        { // Index not in row.
                          // Insert entry in row.
                          for (k = nbrColumns; k > j; k--)
                          {
                            rowMatrixB[k] = rowMatrixB[k - 1];
                          }
                          rowMatrixB[j] = index;
                          nbrColumns++;
                        }
                      }
                    }
                    if ((biV[NumberLength - 1] & 0x40000000L) != 0)
                    {
                      AddBigNbr(biV, TestNbr, biV);
                    }
                    if ((biW[NumberLength - 1] & 0x40000000L) != 0)
                    {
                      AddBigNbr(biW, TestNbr, biW);
                    }
                    NumberLength--;
                    MultBigNbrModN(biV, biW, biU);
                    NumberLength++;
                    if (InsertNewRelation(biR,
                      biT,
                      biU,
                      nbrColumns,
                      matrixB,
                      rowMatrixB,
                      pp,
                      vectLeftHandSide))
                    {
                      partialsFound++;
                      pp++;
                      ShowSIQSStatus(pp, matrixB.length, startTime);
                      if (pp == matrixB.length)
                      {
                        i =
                          EraseSingletons(
                            matrixB,
                            vectLeftHandSide,
                            vectExpParity);
                        if (i != 0)
                        {
                          lowerTextArea.setText(
                            SIQSInfoText + "\n" + i + " singletons discarded");
                          pp -= i;
                        }
                        else
                        {
                          lowerTextArea.setText(
                            SIQSInfoText
                              + "\nSolving congruence matrix using Block Lanczos algorithm");
                          if (LinearAlgebraPhase(NbrPrimes,
                            matrixB,
                            prime,
                            biT,
                            biR,
                            biU,
                            vectExpParity,
                            vectLeftHandSide))
                          {
                            return BigIntToBigNbr(biT); /* Factor found */
                          }
                          else
                          {
                            lowerTextArea.setText(
                              SIQSInfoText
                                + "\nLinear dependences were found. Discarding 50 congruences...");
                            pp -= 50; /* Factor not found */
                          }
                        }
                      }
                      break;
                    }
                    i = -2; // Do not execute next if.
                    break;
                  }
                  prev = i;
                  i = matrixPartial[i][1]; // Get next index for same hash.
                } /* end while */
                if (i == -1 && nbrPartials < matrixPartial.length)
                { // No match.
                  // Add partial to table of partials.
                  if (prev >= 0)
                  {
                    matrixPartial[prev][1] = nbrPartials;
                  }
                  else
                  {
                    matrixPartialHashIndex[(int) (Divid & 0x7FE) / 2] =
                      nbrPartials;
                  }
                  matrixPartial[nbrPartials][0] = (int) Divid;
                           // Indicate last index with this hash.
                  matrixPartial[nbrPartials][1] = -1; 
                  MultBigNbrByLong(biS, index2 - SieveLimit, biT);
                  AddBigNbr(biT, biN, biT); // biT = Ax+B
                  for (index = 0; index < NumberLength; index++)
                  {
                    matrixPartial[nbrPartials][index + 2] = (int) biT[index];
                  }
                  nbrPartials++;
                }
              }
            }
          }
        }
        while (index2 > 0);
        /*******************/
        /* Next polynomial */
        /*******************/
        PolynomialIndex++;
      }
      while (PolynomialIndex < NbrPolynomials);
      PolynomialSetNbr++;
    }
    while (true);
  }

  void ShowSIQSStatus(int pp, int matrixBLength, long startTime)
  {
    long New, u;

    if (TerminateThread)
    {
      throw new ArithmeticException();
    }
    Thread.yield();
    New = System.currentTimeMillis();
    if (OldTimeElapsed >= 0
      && OldTimeElapsed / 1000 != (OldTimeElapsed + New - Old) / 1000)
    {
      OldTimeElapsed += New - Old;
      Old = New;
      long t = OldTimeElapsed / 1000;
      if (New - startTime > 5 && pp > 10)
      {
        u = (New - startTime) * (matrixBLength - pp) / pp / 1000;
        labelStatus.setText(
            GetDHMS(t)
            + "     Congruences found: "
            + pp
            + " ("
            + (int) ((float) (pp * 100) / (float) matrixBLength)
            + "%)   End sieve in "
            + GetDHMS(u));
      }
      else
      {
        labelStatus.setText(
            GetDHMS(t)
            + "     Congruences found: "
            + pp
            + " ("
            + (int) ((float) (pp * 100) / (float) matrixBLength)
            + "%)");
      }
    }
  }

  int EraseSingletons(
    int[][] matrixB,
    long[][] vectLeftHandSide,
    int[] vectExpParity)
  {
    int i, j, delta;
    int[] rowMatrixB;
    // Find singletons in matrixB.
    for (i = matrixB.length - 1; i >= 0; i--)
    {
      vectExpParity[i] = 0;
    }
    for (i = matrixB.length - 1; i >= 0; i--)
    {
      rowMatrixB = matrixB[i];
      for (j = rowMatrixB.length - 1; j >= 0; j--)
      {
        vectExpParity[rowMatrixB[j]]++;
      }
    }
    delta = 0;
    // Erase singletons from matrixB.
    for (i = 0; i < matrixB.length; i++)
    {
      rowMatrixB = matrixB[i];
      for (j = rowMatrixB.length - 1; j >= 0; j--)
      {
        if (vectExpParity[rowMatrixB[j]] == 1)
        { // Singleton found
          delta++;
          break;
        }
      }
      if (j < 0)
      { // Singleton not found
        matrixB[i - delta] = matrixB[i];
        vectLeftHandSide[i - delta] = vectLeftHandSide[i];
      }
    }
    return delta;
  }

  /************************/
  /* Linear algebra phase */
  /************************/
  boolean LinearAlgebraPhase(
    int NbrPrimes,
    int[][] matrixB,
    long[] prime,
    long[] biT,
    long[] biR,
    long[] biU,
    int[] vectExpParity,
    long[][] vectLeftHandSide)
  {
    int mask, i, j, index;
    int[] rowMatrixB;

    int[] matrixV = BlockLanczos(matrixB);
    for (mask = 1; mask != 0; mask *= 2)
    {
      LongToBigNbr(1, biT);
      LongToBigNbr(1, biR);
      for (i = matrixB.length - 1; i >= 0; i--)
      {
        vectExpParity[i] = 0;
      }
      for (i = matrixB.length - 1; i >= 0; i--)
      {
        if ((matrixV[i] & mask) != 0)
        {
          NumberLength--;
          MultBigNbrModN(vectLeftHandSide[i], biR, biU);
          NumberLength++;
          for (j = 0; j < NumberLength; j++)
          {
            biR[j] = biU[j];
          }
          rowMatrixB = matrixB[i];
          NumberLength--;
          for (j = rowMatrixB.length - 1; j >= 0; j--)
          {
            vectExpParity[rowMatrixB[j]] ^= 1;
            if (vectExpParity[rowMatrixB[j]] == 0)
            {
              if (rowMatrixB[j] == 0)
              {
                SubtractBigNbr(TestNbr, biT, biT); // Multiply biT by -1.
              }
              else
              {
                MultBigNbrByLongModN(biT, prime[rowMatrixB[j]], biT);
              }
            }
          }
          NumberLength++;
        }
      }
      for (i = NbrPrimes - 1; i >= 0; i--)
      {
        if (vectExpParity[i] != 0)
        {
          break;
        }
      }
      SubtractBigNbrModN(biR, biT, biR);
      GcdBigNbr(biR, TestNbr2, biT);
      index = 0;
      if (biT[0] == 1)
      {
        for (index = 1; index < NumberLength; index++)
        {
          if (biT[index] != 0)
          {
            break;
          }
        }
      }
      if (index < NumberLength)
      { /* GCD not 1 */
        for (index = 0; index < NumberLength; index++)
        {
          if (biT[index] != TestNbr2[index])
          {
            break;
          }
        }
        if (index < NumberLength)
        { /* GCD not 1 */
          return true;
        }
      }
    }
    return false;
  }

  long modPow(long NbrMod, long Expon, long currentPrime)
  {
    long Power = 1;
    long Square = NbrMod;
    while (Expon != 0)
    {
      if ((Expon & 1) == 1)
      {
        Power = (Power * Square) % currentPrime;
      }
      Square = (Square * Square) % currentPrime;
      Expon /= 2;
    }
    return Power;
  }

  boolean InsertNewRelation(
    long[] biR,
    long[] biT,
    long[] biU,
    int nbrColumns,
    int[][] matrixB,
    int[] rowMatrixB,
    int pp,
    long[][] vectLeftHandSide)
  {
    /* Convert negative numbers to the range 0 <= n < TestNbr */

    int i, k, index;

    if ((TestNbr[0] & 1) == 0)
    {
      DivBigNbrByLong(TestNbr, 2, TestNbr);
      for (i = 0; i < NumberLength; i++)
      {
        biT[i] = 0;
      }
      AddBigNbrModN(biR, biT, biR);
      NumberLength--;
      ModInvBigNbr(biR, biT, TestNbr);
      NumberLength++;
      MultBigNbrByLong(TestNbr, 2, TestNbr);
    }
    else
    {
      ModInvBigNbr(biR, biT, TestNbr);
    }
    if ((biU[NumberLength - 1] & 0x40000000L) != 0)
    {
      AddBigNbr(biU, TestNbr, biU);
    }

    // Compute biU / biR  (mod TestNbr)

    NumberLength--;
    MultBigNbrModN(biU, biT, biR);
    NumberLength++;

    // Insert it only if it is different from previous relations.

    for (i = 0; i < pp; i++)
    {
      if (nbrColumns > matrixB[i].length)
      {
        continue;
      }
      if (nbrColumns == matrixB[i].length)
      {
        for (k = 0; k < nbrColumns; k++)
        {
          if (rowMatrixB[k] != matrixB[i][k])
          {
            break;
          }
        }
        if (k == nbrColumns)
        {
          return false; // Do not insert same relation.
        }
        if (rowMatrixB[k] > matrixB[i][k])
        {
          continue;
        }
      }
      for (k = pp - 1; k >= i; k--)
      {
        matrixB[k + 1] = matrixB[k];
        vectLeftHandSide[k + 1] = vectLeftHandSide[k];
      }
      break;
    }
    matrixB[i] = new int[nbrColumns]; // Add relation to matrix B.
    for (k = 0; k < nbrColumns; k++)
    {
      matrixB[i][k] = rowMatrixB[k];
    }
    vectLeftHandSide[i] = new long[NumberLength];
    for (index = 0; index < NumberLength; index++)
    {
      vectLeftHandSide[i][index] = biR[index];
    }
    return true;
  }
  long modInv(long NbrMod, long currentPrime)
  {
    long QQ, T1, T3;
    long U1 = 1;
    long U3 = NbrMod;
    long V1 = 0;
    long V3 = currentPrime;
    while (V3 != 0)
    {
      QQ = U3 / V3;
      T1 = U1 - V1 * QQ;
      T3 = U3 - V3 * QQ;
      U1 = V1;
      U3 = V3;
      V1 = T1;
      V3 = T3;
    }
    return (U1 + currentPrime) % currentPrime;
  }

  /* Multiply binary matrices of length m x 32 by 32 x 32 */
  /* The product matrix has size m x 32. Then add it to a m x 32 matrix. */
  void MatrixMultAdd(int[] LeftMatr, int[] RightMatr, int[] ProdMatr)
  {
    int leftMatr;
    int matrLength = LeftMatr.length;
    int prodMatr;
    int row, col;
    for (row = 0; row < matrLength; row++)
    {
      prodMatr = ProdMatr[row];
      leftMatr = LeftMatr[row];
      col = 0;
      while (leftMatr != 0)
      {
        if (leftMatr < 0)
        {
          prodMatr ^= RightMatr[col];
        }
        leftMatr *= 2;
        col++;
      }
      ProdMatr[row] = prodMatr;
    }
  }
  /* Multiply binary matrices of length m x 32 by 32 x 32 */
  /* The product matrix has size m x 32 */
  void MatrixMultiplication(int[] LeftMatr, int[] RightMatr, int[] ProdMatr)
  {
    int leftMatr;
    int matrLength = LeftMatr.length;
    int prodMatr;
    int row, col;
    for (row = 0; row < matrLength; row++)
    {
      prodMatr = 0;
      leftMatr = LeftMatr[row];
      col = 0;
      while (leftMatr != 0)
      {
        if (leftMatr < 0)
        {
          prodMatr ^= RightMatr[col];
        }
        leftMatr *= 2;
        col++;
      }
      ProdMatr[row] = prodMatr;
    }
  }

  /* Multiply the transpose of a binary matrix of length n x 32 by */
  /* another binary matrix of length n x 32 */
  /* The product matrix has size 32 x 32 */
  void MatrTranspMult(int[] LeftMatr, int[] RightMatr, int[] ProdMatr)
  {
    int prodMatr;
    int matrLength = LeftMatr.length;
    int row, col;
    int iMask = 1;
    for (col = 31; col >= 0; col--)
    {
      prodMatr = 0;
      for (row = 0; row < matrLength; row++)
      {
        if ((LeftMatr[row] & iMask) != 0)
        {
          prodMatr ^= RightMatr[row];
        }
      }
      ProdMatr[col] = prodMatr;
      iMask *= 2;
    }
  }

  void MatrixAddition(int[] leftMatr, int[] rightMatr, int[] sumMatr)
  {
    for (int row = leftMatr.length - 1; row >= 0; row--)
    {
      sumMatr[row] = leftMatr[row] ^ rightMatr[row];
    }
  }

  void MatrMultBySSt(int[] Matr, int diagS, int[] Prod)
  {
    for (int row = Matr.length - 1; row >= 0; row--)
    {
      Prod[row] = diagS & Matr[row];
    }
  }

  /* Compute Bt * B * input matrix where B is the matrix that holds the */
  /* factorization relations */
  void MultiplyAByMatrix(
    int[][] matrixB,
    int[] Matr,
    int[] TempMatr,
    int[] ProdMatr)
  {
    int index;
    int prodMatr;
    int row, col;
    int[] rowMatrixB;

    /* Compute TempMatr = B * Matr */
    for (row = matrixB.length - 1; row >= 0; row--)
    {
      TempMatr[row] = 0;
    }
    for (row = matrixB.length - 1; row >= 0; row--)
    {
      rowMatrixB = matrixB[row];
      for (index = rowMatrixB.length - 1; index >= 0; index--)
      {
        TempMatr[rowMatrixB[index]] ^= Matr[row];
      }
    }

    /* Compute ProdMatr = Bt * TempMatr */
    for (row = matrixB.length - 1; row >= 0; row--)
    {
      prodMatr = 0;
      rowMatrixB = matrixB[row];
      for (index = rowMatrixB.length - 1; index >= 0; index--)
      {
        prodMatr ^= TempMatr[rowMatrixB[index]];
      }
      ProdMatr[row] = prodMatr;
    }
  }

  void colexchange(int[] XmY, int[] V, int[] V1, int[] V2, int col1, int col2)
  {
    int i;
    int mask1, mask2;
    int[] matr1, matr2;

    if (col1 == col2)
    {
      return;
    }
    mask1 = 1 << (31 - (col1 & 31));
    mask2 = 1 << (31 - (col2 & 31));
    matr1 = (col1 >= 32 ? V1 : V2);
    matr2 = (col2 >= 32 ? V1 : V2);
    for (i = V.length - 1; i >= 0; i--)
    {
      if (((matr1[i] & mask1) == 0) != ((matr2[i] & mask2) == 0))
      {
        matr1[i] ^= mask1;
        matr2[i] ^= mask2;
      }
    }

    matr1 = (col1 >= 32 ? XmY : V);
    matr2 = (col2 >= 32 ? XmY : V);
    for (i = V.length - 1; i >= 0; i--)
    {
      if (((matr1[i] & mask1) == 0) != ((matr2[i] & mask2) == 0))
      {
        matr1[i] ^= mask1;
        matr2[i] ^= mask2;
      }
    }
  }

  void coladd(int[] XmY, int[] V, int[] V1, int[] V2, int col1, int col2)
  {
    int i;
    int mask1, mask2;
    int[] matr1, matr2;

    if (col1 == col2)
    {
      return;
    }
    mask1 = 1 << (31 - (col1 & 31));
    mask2 = 1 << (31 - (col2 & 31));
    matr1 = (col1 >= 32 ? V1 : V2);
    matr2 = (col2 >= 32 ? V1 : V2);
    for (i = V.length - 1; i >= 0; i--)
    {
      if ((matr1[i] & mask1) != 0)
      {
        matr2[i] ^= mask2;
      }
    }

    matr1 = (col1 >= 32 ? XmY : V);
    matr2 = (col2 >= 32 ? XmY : V);
    for (i = V.length - 1; i >= 0; i--)
    {
      if ((matr1[i] & mask1) != 0)
      {
        matr2[i] ^= mask2;
      }
    }
  }

  int[] BlockLanczos(int[][] matrixB)
  {
    int i, j, k = matrixB.length;
    int oldDiagonalSSt, newDiagonalSSt;
    int index, indexC, mask;
    int[] matrixD = new int[32];
    int[] matrixE = new int[32];
    int[] matrixF = new int[32];
    int[] matrixWinv = new int[32];
    int[] matrixWinv1 = new int[32];
    int[] matrixWinv2 = new int[32];
    int[] matrixVtV0 = new int[32];
    int[] matrixVt1V0 = new int[32];
    int[] matrixVt2V0 = new int[32];
    int[] matrixVtAV = new int[32];
    int[] matrixVt1AV1 = new int[32];
    int[] matrixAV = new int[k];
    int[] matrixCalcParenD = new int[32];
    int[] vectorIndex = new int[64];
    int[] matrixV = new int[k];
    int[] matrixV1 = new int[k];
    int[] matrixV2 = new int[k];
    int[] matrixXmY = new int[k];
    int[] matrixCalc3 = new int[k]; // Matrix that holds temporary data
    int[] matrixTemp;
    int[] matrixCalc1 = new int[32]; // Matrix that holds temporary data
    int[] matrixCalc2 = new int[32]; // Matrix that holds temporary data
    int[] matr;
    long seed;
    int Temp, Temp1;
    int dimension = 0;
    int stepNbr = 0;
    int currentOrder, currentMask;
    int dim, maxdim;
    int minind, min, minanz;
    int[] rowMatrixB;

    newDiagonalSSt = oldDiagonalSSt = -1;

    /* Initialize matrix X-Y with random data */
    seed = 123456789L;
    for (i = matrixXmY.length - 1; i >= 0; i--)
    {
      matrixXmY[i] = (int) seed;
      seed = (seed * 62089911L + 54325442L) % DosALa31_1;
      matrixXmY[i] += (int) (seed * 6543265L);
      seed = (seed * 62089911L + 54325442L) % DosALa31_1;
    }
    // Compute matrix V(0) = A(X-Y)
    MultiplyAByMatrix(matrixB, matrixXmY, matrixCalc3, matrixV);
    // Compute matrix Vt(0) * V(0)
    MatrTranspMult(matrixV, matrixV, matrixVtV0);
    while (true)
    {
      oldDiagonalSSt = newDiagonalSSt;
      stepNbr++;
      // Compute matrix A * V(i)
      MultiplyAByMatrix(matrixB, matrixV, matrixCalc3, matrixAV);
      // Compute matrix Vt(i) * A * V(i)
      MatrTranspMult(matrixV, matrixAV, matrixVtAV);

      /* If Vt(i) * A * V(i) = 0, end of loop */
      for (i = matrixVtAV.length - 1; i >= 0; i--)
      {
        if (matrixVtAV[i] != 0)
        {
          break;
        }
      }
      if (i < 0)
      {
        break;
      } /* End X-Y calculation loop */

      /* Selection of S(i) and W(i) */

      matrixTemp = matrixWinv2;
      matrixWinv2 = matrixWinv1;
      matrixWinv1 = matrixWinv;
      matrixWinv = matrixTemp;

      mask = 1;
      for (j = 31; j >= 0; j--)
      {
        matrixD[j] = matrixVtAV[j]; /*  D = VtAV    */
        matrixWinv[j] = mask; /*  Winv = I    */
        mask *= 2;
      }

      index = 31;
      indexC = 31;
      for (mask = 1; mask != 0; mask *= 2)
      {
        if ((oldDiagonalSSt & mask) != 0)
        {
          matrixE[index] = indexC;
          matrixF[index] = mask;
          index--;
        }
        indexC--;
      }
      indexC = 31;
      for (mask = 1; mask != 0; mask *= 2)
      {
        if ((oldDiagonalSSt & mask) == 0)
        {
          matrixE[index] = indexC;
          matrixF[index] = mask;
          index--;
        }
        indexC--;
      }
      newDiagonalSSt = 0;
      for (j = 0; j < 32; j++)
      {
        currentOrder = matrixE[j];
        currentMask = matrixF[j];
        for (k = j; k < 32; k++)
        {
          if ((matrixD[matrixE[k]] & currentMask) != 0)
          {
            break;
          }
        }
        if (k < 32)
        {
          i = matrixE[k];
          Temp = matrixWinv[i];
          matrixWinv[i] = matrixWinv[currentOrder];
          matrixWinv[currentOrder] = Temp;
          Temp1 = matrixD[i];
          matrixD[i] = matrixD[currentOrder];
          matrixD[currentOrder] = Temp1;
          newDiagonalSSt |= currentMask;
          dimension++;
          for (k = 31; k >= 0; k--)
          {
            if (k != currentOrder)
            {
              if ((matrixD[k] & currentMask) != 0)
              {
                matrixWinv[k] ^= Temp;
                matrixD[k] ^= Temp1;
              }
            }
          } /* end for k */
        }
        else
        {
          for (k = j; k < 32; k++)
          {
            if ((matrixWinv[matrixE[k]] & currentMask) != 0)
            {
              break;
            }
          }
          i = matrixE[k];
          Temp = matrixWinv[i];
          matrixWinv[i] = matrixWinv[currentOrder];
          matrixWinv[currentOrder] = Temp;
          Temp1 = matrixD[i];
          matrixD[i] = matrixD[currentOrder];
          matrixD[currentOrder] = Temp1;
          for (k = 31; k >= 0; k--)
          {
            if ((matrixWinv[k] & currentMask) != 0)
            {
              matrixWinv[k] ^= Temp;
              matrixD[k] ^= Temp1;
            }
          } /* end for k */
        } /* end if */
      } /* end for j */
      /* Compute D(i), E(i) and F(i) */
      if (oldDiagonalSSt != -1)
      { /* S=I => F=0 */
        // F = -Winv(i-2) * (I - Vt(i-1)*A*V(i-1)*Winv(i-1)) * ParenD * S*St
        MatrixMultiplication(matrixVt1AV1, matrixWinv1, matrixCalc2);
        index = 31; /* Add identity matrix */
        for (mask = 1; mask != 0; mask *= 2)
        {
          matrixCalc2[index] ^= mask;
          index--;
        }
        MatrixMultiplication(matrixWinv2, matrixCalc2, matrixCalc1);
        MatrixMultiplication(matrixCalc1, matrixCalcParenD, matrixF);
        MatrMultBySSt(matrixF, newDiagonalSSt, matrixF);
      }
      // E = -Winv(i-1) * Vt(i)*A*V(i) * S*St
      MatrixMultiplication(matrixWinv1, matrixVtAV, matrixE);
      MatrMultBySSt(matrixE, newDiagonalSSt, matrixE);

      // ParenD = Vt(i)*A*A*V(i) * S*St + Vt(i)*A*V(i)
      // D = I - Winv(i) * ParenD
      MatrTranspMult(matrixAV, matrixAV, matrixCalc1); // Vt(i)*A*A*V(i)
      MatrMultBySSt(matrixCalc1, newDiagonalSSt, matrixCalc1);
      MatrixAddition(matrixCalc1, matrixVtAV, matrixCalcParenD);
      MatrixMultiplication(matrixWinv, matrixCalcParenD, matrixD);
      index = 31; /* Add identity matrix */
      for (mask = 1; mask != 0; mask *= 2)
      {
        matrixD[index] ^= mask;
        index--;
      }

      /* Update value of X - Y */
      MatrixMultiplication(matrixWinv, matrixVtV0, matrixCalc1);
      MatrixMultAdd(matrixV, matrixCalc1, matrixXmY);

      /* Compute value of new matrix V(i) */
      // V(i+1) = A * V(i) * S * St + V(i) * D + V(i-1) * E + V(i-2) * F
      MatrMultBySSt(matrixAV, newDiagonalSSt, matrixCalc3);
      MatrixMultAdd(matrixV, matrixD, matrixCalc3);
      MatrixMultAdd(matrixV1, matrixE, matrixCalc3);
      if (oldDiagonalSSt != -1)
      { // F != 0
        MatrixMultAdd(matrixV2, matrixF, matrixCalc3);
      }

      /* Compute value of new matrix Vt(i)V0 */
      if (stepNbr > 3)
      {
        // Vt(i+1)V(0) = Dt * Vt(i)V(0) + Et * Vt(i-1)V(0) + Ft * Vt(i-2)V(0)
        MatrTranspMult(matrixD, matrixVtV0, matrixCalc1);
        MatrTranspMult(matrixE, matrixVt1V0, matrixCalc2);
        MatrixAddition(matrixCalc1, matrixCalc2, matrixCalc2);
        if (oldDiagonalSSt != -1)
        { // F != 0
          MatrTranspMult(matrixF, matrixVt2V0, matrixCalc1);
          MatrixAddition(matrixCalc1, matrixCalc2, matrixCalc2);
        }
      }
      else
      {
        if (stepNbr == 1)
        {
          MatrTranspMult(matrixCalc3, matrixV, matrixCalc2); // V(1)t * V(0)
        }
        else
        { // if stepNbr == 2 ...
          MatrTranspMult(matrixCalc3, matrixV1, matrixCalc2); // V(2)t * V(0)
        }
      }
      matrixTemp = matrixV2;
      matrixV2 = matrixV1;
      matrixV1 = matrixV;
      matrixV = matrixCalc3;
      matrixCalc3 = matrixTemp;
      matrixTemp = matrixVt2V0;
      matrixVt2V0 = matrixVt1V0;
      matrixVt1V0 = matrixVtV0;
      matrixVtV0 = matrixCalc2;
      matrixCalc2 = matrixTemp;
      matrixTemp = matrixVt1AV1;
      matrixVt1AV1 = matrixVtAV;
      matrixVtAV = matrixTemp;
    } /* end while */

    /* Find matrix V1:V2 = B * (X-Y:V) */ {
      int row;
      for (row = matrixB.length - 1; row >= 0; row--)
      {
        matrixV1[row] = matrixV2[row] = 0;
      }
      for (row = matrixB.length - 1; row >= 0; row--)
      {
        rowMatrixB = matrixB[row];
        for (index = rowMatrixB.length - 1; index >= 0; index--)
        {
          matrixV1[rowMatrixB[index]] ^= matrixXmY[row];
          matrixV2[rowMatrixB[index]] ^= matrixV[row];
        }
      }
    }
    maxdim = 64;
    dim = 0;
    while (dim < maxdim)
    {
      for (i = dim; i < maxdim; i++)
      {
        matr = (i >= 32 ? matrixV1 : matrixV2);
        mask = 1 << (31 - (i & 31));
        vectorIndex[i] = -1;
        for (j = 0; j < matrixV1.length; j++)
        {
          if ((matr[j] & mask) != 0)
          {
            vectorIndex[i] = j;
            break;
          }
        }
      }
      for (i = dim; i < maxdim; i++)
      {
        if (vectorIndex[i] < 0)
        {
          colexchange(matrixXmY, matrixV, matrixV1, matrixV2, dim, i);
          vectorIndex[i] = vectorIndex[dim];
          vectorIndex[dim] = -1;
          dim++;
        }
      }
      if (dim == maxdim)
      {
        break;
      }
      min = vectorIndex[dim];
      minind = dim;
      for (i = dim + 1; i < maxdim; i++)
      {
        if (vectorIndex[i] < min)
        {
          min = vectorIndex[i];
          minind = i;
        }
      }
      minanz = 0;
      for (i = dim; i < maxdim; i++)
      {
        if (vectorIndex[i] == min)
        {
          minanz++;
        }
      }
      if (minanz > 1)
      {
        for (i = minind + 1; i < maxdim; i++)
        {
          if (vectorIndex[i] == min)
          {
            coladd(matrixXmY, matrixV, matrixV1, matrixV2, minind, i);
          }
        }
      }
      else
      {
        maxdim--;
        colexchange(matrixXmY, matrixV, matrixV1, matrixV2, minind, maxdim);
      }
    }
    dim = 0; /* find linear independent solutions */
    while (dim < maxdim)
    {
      for (i = dim; i < maxdim; i++)
      {
        matr = (i >= 32 ? matrixXmY : matrixV);
        mask = 1 << (31 - (i & 31));
        vectorIndex[i] = -1;
        for (j = 0; j < matrixV1.length; j++)
        {
          if ((matr[j] & mask) != 0)
          {
            vectorIndex[i] = j;
            break;
          }
        }
      }
      for (i = dim; i < maxdim; i++)
      {
        if (vectorIndex[i] < 0)
        {
          maxdim--;
          colexchange(matrixXmY, matrixV, matrixV1, matrixV2, maxdim, i);
          vectorIndex[i] = vectorIndex[maxdim];
          vectorIndex[maxdim] = -1;
        }
      }
      if (dim == maxdim)
      {
        break;
      }
      min = vectorIndex[dim];
      minind = dim;
      for (i = dim + 1; i < maxdim; i++)
      {
        if (vectorIndex[i] < min)
        {
          min = vectorIndex[i];
          minind = i;
        }
      }
      minanz = 0;
      for (i = dim; i < maxdim; i++)
      {
        if (vectorIndex[i] == min)
        {
          minanz++;
        }
      }
      if (minanz > 1)
      {
        for (i = minind + 1; i < maxdim; i++)
        {
          if (vectorIndex[i] == min)
          {
            coladd(matrixXmY, matrixV, matrixV1, matrixV2, minind, i);
          }
        }
      }
      else
      {
        colexchange(matrixXmY, matrixV, matrixV1, matrixV2, minind, dim);
        dim++;
      }
    }
    return matrixV;
  }

  public String StartFactorExprBatch(String inputStr, int type)
  {
    batchFinished = false;
    this.inputStr = inputStr;
    batchPrime = (type == 1);
    calcThread = new Thread(this);
    calcThread.start();
    return "";
  }

  void BatchThread()
  {
    outputStr = new StringBuffer();
    String expr;
    int StrLen = inputStr.length();
    int i, index = 0;
    int newIndex;
    int ExpressionRC;
    BigInteger ExpressionResult[] = new BigInteger[1];
    textNumber.setEditable(false);
    labelTop.setText("Number to factor:");
    while (index < StrLen)
    {
      newIndex = inputStr.indexOf('\n', index);
      if (newIndex < 0)
      {                // No line feed found, it means last line.
        newIndex = StrLen;
      }
      expr = inputStr.substring(index, newIndex).trim();
      index = newIndex + 1;
      if (expr == "")
      {
        outputStr.append("\n");
      }
      else
      {
        try
        {
          ExpressionRC =
            expression.ComputeExpression(
              expr,
              0,
              ExpressionResult);
        }
        catch (OutOfMemoryError e)
        {
          outputStr.append(expr+": Out of memory\n");
          continue;
        }
        catch (ArithmeticException e)
        {
          continue;
        }
        NumberToFactor = ExpressionResult[0];
        if (ExpressionRC != 0)
        {
          outputStr.append(expr+": "+expressionText[-1 - ExpressionRC]+"\n");
          continue;
        }
        if (batchPrime)
        {
          if (NumberToFactor.compareTo(BigInt3) <= 0)
          {
            NbrFactors = 0;     // Indicate it is prime (2 or 3).
          }
          else if (BigInt3.modPow(NumberToFactor.subtract(BigInt1),
                        NumberToFactor).equals(BigInt1))
          {                // Pseudoprime
            if (NumberToFactor.bitLength() < 34)
            {                    // Small number
              long modulus = NumberToFactor.longValue();
              if (modulus % 2 == 0)
              {
                NbrFactors = 1;     // Indicate it is composite.
              }
              else
              {
                NbrFactors = 0;     // Indicate prime in advance.
                for (long Div = 3; Div*Div <= modulus; Div += 2)
                {
                  if (modulus % Div == 0)
                  {
                    NbrFactors = 1;     // Indicate it is composite.
                    break;
                  }
                }
              }
            }
            else
            {                      // Large number.
              textNumber.setText(NumberToFactor.toString());
              startNewFactorization(true); // Request complete factorization
              if (NbrFactors == 1 && Exp[0] == 1)
              {
                NbrFactors = 0;    // Indicate number prime
              }
            }
          }
          else
          {                        // Pseudoprime test indicate composite.
            NbrFactors = 1;        // Indicate number composite.
          }
        }
        else
        {
          if (NumberToFactor.bitLength() < 34)
          {                    // Small number
            long modulus = NumberToFactor.longValue();
            int Expon = 0;
            i = 0;
            while (modulus % 2 == 0)
            {
              Expon++;
              modulus /= 2;
            }
            if (Expon > 0)
            {
              PD[0] = BigInt2;
              Exp[0] = Expon;
              i++;
            }
            long Div = 3;
            while (Div*Div <= modulus)
            {
              Expon = 0;
              while (modulus % Div == 0)
              {
                Expon++;
                modulus /= Div;
              }
              if (Expon > 0)
              {
                PD[i] = BigInteger.valueOf(Div);
                Exp[i] = Expon;
                i++;
              }
              Div += 2;
            }
            if (modulus > 1)
            {
              PD[i] = BigInteger.valueOf(modulus);
              Exp[i] = 1;
              i++;
            }
            NbrFactors = i;
          }
          else
          {
            textNumber.setText(NumberToFactor.toString());
            startNewFactorization(true);     // Request complete factorization
          }
        }
        outputStr.append(NumberToFactor.toString());
        if (batchPrime)
        {               // Test primality
          if (NbrFactors == 0)
          {
            outputStr.append(" is prime\n");
          }
          else
          {
            outputStr.append(" is composite\n");
          }
        }
        else
        {               // Perform complete factorization
          outputStr.append(" = ");
          for (i=0; i<NbrFactors; i++)
          {
            if (i > 0)
            {
              outputStr.append(" * ");             
            }
            outputStr.append(PD[i].toString());
            if (Exp[i] > 1)
            {
              outputStr.append("^");
              outputStr.append(Exp[i]);
            }
          }
          outputStr.append("\n");
        }
      }
    }
    batchFinished = true;
    onlyFactoring = true;
    textNumber.setEditable(true);
    textNumber.setText("");
    upperTextArea.setText("");
    lowerTextArea.setText("");
    labelStatus.setText("");
    labelTop.setText
     ("Type number or numerical expression to factor here and press Return:");
  }

  public String resultBatch()
  {
    if (batchFinished)
    {
      return outputStr.toString();
    }
    return "";
  }
  class Command implements ActionListener
  {
    int id;
    ecm appletEcm;

    public Command(int id, ecm appletEcm)
    {
      this.id = id;
      this.appletEcm = appletEcm;
    }

    public void actionPerformed(ActionEvent e)
    {
      int nbrEntered;
      switch (id)
      {
        case actionTextNbr :
          if (calcThread != null && calcThread.isAlive())
          {
            new AlertContinue(appletEcm); // Pop up alert
          }
          else
          {
            startNewFactorization(false);    // Start new factorization.
          }
          break;
        case actionTextCurve :
        case actionBtnCurve :
          try
          {
            nbrEntered = Integer.parseInt(textCurve.getText());
          }
          catch (Exception exc)
          {
            nbrEntered = -1;
          }
          if (nbrEntered >= 0 && nbrEntered < 100000000)
          {
            NextEC = nbrEntered;
            if (calcThread != null)
            {
              TerminateThread = true;
              try
              {
                // Wait until the factorization thread dies
                calcThread.join();
              }
              catch (InterruptedException ie)
              {
              };
            }
            if (NextEC == 0)
            {
              NextEC = TYP_SIQS + EC;
            }
            calcThread = new Thread(appletEcm); // Start factorization thread
            calcThread.start();
          }
          break;
        case actionTextFactor :
        case actionBtnFactor :
          try
          {
            InputFactor = new BigInteger(textFactor.getText().trim());
          }
          catch (Exception exc)
          {
            InputFactor = BigInt0;
          }

          if (InputFactor.compareTo(BigInt1) > 0)
          {
            if (calcThread != null)
            {
              TerminateThread = true;
              try
              {
                // Wait until the factorization thread dies
                calcThread.join();
              }
              catch (InterruptedException ie)
              {
              };
            }
            InsertNewFactor(InputFactor);
            NextEC = EC;
            calcThread = new Thread(appletEcm); // Start factorization thread
            calcThread.start();
          }
          break;
      } // end switch
    } // end method
  } // end inner class

} /* end applet */

class AlertContinue extends Frame
{

  private Label label1, label2;
  private Button buttonYes, buttonNo;
  private ecm appletEcm;

  public AlertContinue(ecm applet)
  {
    super("Warning!");
    Dimension dimension;
    resize(400, 210);
    show();
    setLayout(null);
    label1 = new Label("A factorization is still in progress!", Label.CENTER);
    label2 =
      new Label("Do you want to stop it and start a new one?", Label.CENTER);
    label1.setFont(new Font("Helvetica", Font.PLAIN, 12));
    label2.setFont(new Font("Helvetica", Font.PLAIN, 12));
    buttonYes = new Button("Yes");
    buttonNo = new Button("No");
    label1.reshape(25, 40, 350, 20);
    label2.reshape(25, 70, 350, 20);
    buttonYes.reshape(100, 130, 40, 30);
    buttonNo.reshape(260, 130, 40, 30);
    add(label1);
    add(label2);
    add(buttonYes);
    add(buttonNo);
    buttonNo.requestFocus();
    validate();
    appletEcm = applet;
  }

  public boolean handleEvent(Event e)
  {
    if (e.id == Event.ACTION_EVENT && e.target == buttonYes)
    {
      appletEcm.startNewFactorization(false);  // Start new factorization.
      super.dispose();
    }
    if (e.id == Event.ACTION_EVENT
      && e.target == buttonNo
      || e.id == Event.WINDOW_DESTROY)
    {
      super.dispose();
    }
    return (super.handleEvent(e));
  }
} /* end class */
// </XMP>
