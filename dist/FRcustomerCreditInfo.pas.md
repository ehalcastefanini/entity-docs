<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustomerCreditInfo`

## 1. Overview:

### Objective and Problem Solved:
The `FRcustomerCreditInfo` unit is designed to display and manage customer credit information in a structured and interactive form. It provides a detailed view of customer credit data, including overdue amounts, invoices, orders, and available credit in both local and foreign currencies. This form is part of a larger system that handles customer financial data, enabling users to view and interact with credit-related information efficiently.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface.
- **SOAP Services**: For communication with external services.
- **Database Components**: For binding and displaying data from a database.
- **Custom Components**: Includes `TsPanel`, `TsLabel`, `TsDBEdit`, and others from the `s` library for enhanced UI styling.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Labels (`TsLabel`): Display static text.
  - Text Fields (`TsDBEdit`, `TsDBText`): Display and allow editing of database-bound data.
  - Panels (`TsPanel`): Group related UI elements.
  - Buttons (`TsBitBtn`): Trigger actions like "Add," "Apply," and "Cancel."
- **Form Actions and Effects**:
  - **Add Button**: Allows adding new data.
  - **Apply Button**: Saves changes to the database.
  - **Cancel Button**: Cancels the current operation.

---

## 2. Functionality Description:

### User Actions:
- View customer credit information, including overdue amounts, invoices, and available credit.
- Edit specific fields related to customer credit.
- Save or cancel changes made to the data.

### Main Components:
- **Panels (`TsPanel`)**: Organize the layout and group related fields.
- **Labels (`TsLabel`)**: Provide descriptions for fields.
- **Database Fields (`TsDBEdit`, `TsDBText`)**: Display and allow editing of database-bound data.
- **Buttons (`TsBitBtn`)**: Trigger actions like saving or canceling changes.

### Pseudo-code for Actions and Events:
- `OnClick` event of "Add" button: `if add button clicked then open new record form`.
- `OnClick` event of "Apply" button: `if apply button clicked then save changes to database`.
- `OnClick` event of "Cancel" button: `if cancel button clicked then discard changes`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings in the `Create` constructor.
   - Database fields are bound to the form elements.
   - Panels and actions are configured for visibility and availability.

2. **User Interactions**:
   - Users can view and edit customer credit data.
   - Buttons trigger actions like saving or canceling changes.

3. **Functions**:
   - `ShowCreditInfo`: Displays credit information.
   - `m_SetAccessMode`: Configures access mode for the form.

### Data Requirements:
- Users must provide or edit the following data:
  - Overdue amounts.
  - Invoice details.
  - Available credit.
  - Currency codes.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Button**: Enabled when the user wants to add a new record.
- **Apply Button**: Enabled only if changes have been made to the data.
- **Cancel Button**: Enabled when there are unsaved changes.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

### `ShowCreditInfo`:
- Displays customer credit information on the form.

### `m_SetAccessMode`:
- Configures the access mode for the form, determining which actions are available.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Not explicitly defined in the code.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based communication.
- **DBClient**: For database operations.
- **sComponents**: Custom UI components for enhanced styling.

### Custom Components:
- `TsPanel`, `TsLabel`, `TsDBEdit`, `TsDBText`, `TsBitBtn`: Used for UI design and interaction.

---

## 9. Fields and Validations Listing:

### Fields:
- `EDTshipStat` (type: string, database-bound).
- `EDTordersEUR` (type: currency, database-bound).
- `EDTinvoicesEUR` (type: currency, database-bound).
- `EDToutInvoicesEUR` (type: currency, database-bound).
- `EDTworkInProgEUR` (type: currency, database-bound).
- `EDTavailCredEUR` (type: currency, database-bound).
- `EDTnetOutInvEur` (type: currency, database-bound).
- `EDTcurrencyCode` (type: string, database-bound).
- `EDToverdue1_15` (type: currency, database-bound).
- `EDToverdue15_30` (type: currency, database-bound).
- `EDToverdue31_60` (type: currency, database-bound).
- `EDToverdue61_90` (type: currency, database-bound).
- `EDToverdueM90` (type: currency, database-bound).
- `EDToverdueTotal` (type: currency, database-bound).

### Mapping:
- Displayed values are directly bound to database columns.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
constructor TFRAMEcustomerCreditInfo.Create(AOwner: TComponent);
begin
  inherited;
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerCredit';
  PropertyName := 'credit';
  FrameType := frtDetail;
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;
  OnSetAccessMode := m_SetAccessMode;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- `// SET DAS PROPRIEDADES DA FRAME`: Indicates where frame properties are set.
- `// configurar visibilidade de painel de ações e ações disponíveis`: Configures the visibility of action panels and available actions.

---

## 12. Conclusion:

The `FRcustomerCreditInfo` unit provides a structured and interactive form for managing customer credit information. While it effectively displays and allows editing of credit data, the code lacks explicit error handling, field validation, and API integration details. Its strengths lie in its modular design and use of custom components for enhanced UI.

---

## 13. Short Summary:

The `FRcustomerCreditInfo` unit is a Delphi-based form for managing customer credit data, featuring database-bound fields and custom UI components. It supports viewing and editing credit information but lacks explicit error handling and validation logic.#### **FRcustomerCreditInfo.pas**

```
unit FRcustomerCreditInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, StdCtrls, Mask, DBCtrls,
  sDBEdit, sDBText, sLabel, sFrameAdapter, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, Buttons, sBitBtn, sPanel, sBevel;

type
  TFRAMEcustomerCreditInfo = class(TFRAMEBaseCtrlEditSOA)
    PNLshipStat: TsPanel;     //#23764 (cmosilva 10-10-2019)
    LBLshipStat: TsLabel;
    EDTshipStat: TsDBEdit;
    BVL1: TsBevel;
    PNLcurr: TsPanel;          //#23764 (cmosilva 10-10-2019)
    PNLcurrLocal1: TsPanel;    //#23764 (cmosilva 10-10-2019)
    PNLcurrForeign1: TsPanel;  //#23764 (cmosilva 10-10-2019)
    PNLlblOverdue: TsPanel;    //#23764 (cmosilva 10-10-2019)
    LBLLabel11: TsLabel;
    LBLLabel13: TsLabel;
    LBLLabel15: TsLabel;
    LBLLabel17: TsLabel;
    LBLLabel18: TsLabel;
    LBLLabel19: TsLabel;
    LBLnetOutInv: TsLabel;
    EDTordersEUR: TsDBEdit;
    EDTinvoicesEUR: TsDBEdit;
    EDToutInvoicesEUR: TsDBEdit;
    EDTworkInProgEUR: TsDBEdit;
    EDTavailCredEUR: TsDBEdit;
    EDTnetOutInvEur: TsDBEdit;
    EDTcurrencyCode: TsDBText;
    EDTordersCUR: TsDBEdit;
    EDToutInvoicesCUR: TsDBEdit;
    EDTinvoicesCUR: TsDBEdit;
    EDTworkInProgCUR: TsDBEdit;
    EDTavailCredCUR: TsDBEdit;
    EDTnetOutInvCUR: TsDBEdit;
    PNLlocalCurr2: TsPanel;     //#23764 (cmosilva 10-10-2019)
    PNLcurrForeign2: TsPanel;   //#23764 (cmosilva 10-10-2019)
    LBLov1630: TsLabel;
    LBLov3160: TsLabel;
    LBLov6190: TsLabel;
    LBLov90: TsLabel;
    LBLovTotal: TsLabel;
    LBLov115: TsLabel;
    LBLov: TsLabel;
    LBLovEurHeader: TsLabel;
    EDT1: TsDBEdit;
    EDT2: TsDBEdit;
    EDT3: TsDBEdit;
    EDT4: TsDBEdit;
    EDT5: TsDBEdit;
    EDT6: TsDBEdit;
    DBTXT1: TsDBText;
    EDToverdue1_15: TsDBEdit;
    EDToverdue15_30: TsDBEdit;
    EDToverdue31_60: TsDBEdit;
    EDToverdue61_90: TsDBEdit;
    EDToverdueM90: TsDBEdit;
    EDToverdueTotal: TsDBEdit;
    procedure CDStableAfterOpen(DataSet: TDataSet);
  private
    { Private declarations }
    procedure ShowCreditInfo;
    procedure m_SetAccessMode(Sender: TObject; var pv_stat: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerCreditInfo: TFRAMEcustomerCreditInfo;

implementation

{$R *.dfm}
uses
  kneTypes;

constructor TFRAMEcustomerCreditInfo.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerCredit';
  PropertyName := 'credit';
  FrameType := frtDetail;


  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  OnSetAccessMode := m_SetAccessMode;
```

#### **FRcustomerCreditInfo.dfm**

```
inherited FRAMEcustomerCreditInfo: TFRAMEcustomerCreditInfo
  Width = 762
  Height = 249
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 762
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

