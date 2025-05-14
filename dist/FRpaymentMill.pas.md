<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRpaymentMill` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRpaymentMill` code unit defines a Delphi frame (`TFRAMEpaymentMill`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display payment-related data for mills in a grid format. It provides functionalities for editing, searching, and managing mill payment data, including integration with external services for SAP and mill-related information.

This frame is designed to be part of a larger system, likely an enterprise application, where users can interact with payment data in a structured and user-friendly grid interface.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **cxGrid**: A component for displaying and managing data in a grid format.
- **SOAP Services**: For integration with external services (`MillServiceUtils`, `SapPaymentServiceUtils`).
- **Database Components**: For managing and interacting with datasets.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **mill**: Custom editor (`cxEDTfind`).
2. **millPayment**: Custom editor (`cxEDTupperCase`).
3. **millDescrip**: Custom editor (`cxEDTupperCase`).
4. **sapPayment**: Custom editor (`cxEDTfindSAP`).
5. **stat**: Custom editor (`cxEDTstat`).

#### Grid Actions and Their Effects:
- **Add**: Allows users to add new entries to the grid.
- **Edit**: Enables editing of existing grid entries.
- **Search**: Provides search functionality for mill and SAP payment data.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add New Entry**: Users can add new payment data for mills.
2. **Edit Existing Data**: Users can modify existing entries in the grid.
3. **Search for Mill or SAP Payment**: Users can search for specific mill or SAP payment data using custom editors.

### Main Components:
- **Grid (`cxGrid`)**: Displays the payment data.
- **Custom Editors**: Specialized editors for fields like `mill`, `millPayment`, and `sapPayment`.
- **Action Panel**: Provides buttons for actions like "Add."

### Pseudo-code for Actions and Events:
- `OnButtonClick` event for `cxEDTfind`:
  - `if button clicked then execute m_SetFindMill`.
- `OnButtonClick` event for `cxEDTfindSAP`:
  - `if button clicked then execute m_SetFindSAP`.
- `OnEditValueChanged` event for grid:
  - `if grid cell value changed then execute cxDBVtableEditValueChanged`.
- `OnExecute` event for "Add" action:
  - `if add button clicked then execute ACTaddExecute`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized in the `Create` constructor.
   - Grid settings, hidden fields, column order, key fields, and custom editors are configured.
   - Event handlers for custom editors are assigned.

2. **User Interactions**:
   - Users interact with the grid to add, edit, or search for data.
   - Clicking buttons triggers specific event handlers.

### Functions and File Locations:
- **`Create`** (in `FRpaymentMill`): Initializes the frame and configures grid settings.
- **`m_SetFindMill`** (in `FRpaymentMill`): Handles mill search functionality.
- **`m_SetFindSAP`** (in `FRpaymentMill`): Handles SAP payment search functionality.
- **`cxDBVtableEditValueChanged`** (in `FRpaymentMill`): Handles changes in grid cell values.
- **`ACTaddExecute`** (in `FRpaymentMill`): Handles the "Add" action.

### Required User Data:
- Mill code, payment details, and descriptions.
- SAP payment information.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Action**: Enabled by default. No preconditions are specified.
- **Edit Action**: Triggered when a grid cell value is changed.
- **Search Action**: Requires user interaction with the custom editors.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **`millPayment`**: Converts input to uppercase.
- **`millDescrip`**: Converts input to uppercase.
- **`sapPayment`**: Converts input to uppercase and provides a search button.

---

## 5. Main Functions:

1. **`Create`**: Initializes the frame and configures grid settings.
2. **`m_SetFindMill`**: Handles mill search functionality.
3. **`m_SetFindSAP`**: Handles SAP payment search functionality.
4. **`cxDBVtableEditValueChanged`**: Handles changes in grid cell values.
5. **`ACTaddExecute`**: Handles the "Add" action.

---

## 6. API Service Consumption:

### External Service Calls:
1. **MillServiceUtils**:
   - Purpose: Fetch mill-related data.
   - Endpoint: Not explicitly defined in the code.
2. **SapPaymentServiceUtils**:
   - Purpose: Fetch SAP payment-related data.
   - Endpoint: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **`sapPayment` Field**:
  - Appears with a search button for SAP payment data.
  - Condition: Always visible.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **SOAPHTTPClient**: For SOAP service integration.
- **DBClient**: For dataset management.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: Base class for the frame.
- **Custom Editors**: `cxEDTfind`, `cxEDTupperCase`, `cxEDTfindSAP`.

---

## 9. Fields and Validations Listing:

1. **mill**:
   - Type: String.
   - Editor: `cxEDTfind`.
   - Validation: Not explicitly defined.
2. **millPayment**:
   - Type: String.
   - Editor: `cxEDTupperCase`.
   - Validation: Converts input to uppercase.
3. **millDescrip**:
   - Type: String.
   - Editor: `cxEDTupperCase`.
   - Validation: Converts input to uppercase.
4. **sapPayment**:
   - Type: String.
   - Editor: `cxEDTfindSAP`.
   - Validation: Converts input to uppercase.
5. **stat**:
   - Type: String.
   - Editor: `cxEDTstat`.
   - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEpaymentMill.m_SetFindMill(Sender: TObject; AButtonIndex: Integer);
begin
  // Logic for mill search
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **`MasterKeyFields`**: Defines the relationship between payment codes and GPS payments.
- **`DataPacketName`**: Specifies the name of the detail in the metadata.
- **`AvailableActions`**: Configures available actions for the frame.

---

## 12. Conclusion:

The `FRpaymentMill` code unit provides a robust framework for managing mill payment data in a grid format. Its strengths include integration with external services and customizable grid settings. However, the lack of explicit error handling and validation logic may limit its reliability in certain scenarios.

---

## 13. Short Summary:

The `FRpaymentMill` unit defines a grid-based interface for managing mill payment data, with features for adding, editing, and searching. It integrates with external services and provides customizable grid settings, making it suitable for enterprise applications.#### **FRpaymentMill.pas**

```
unit FRpaymentMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEpaymentMill = class(TFRAMEBaseGridEditSOA)
    cxEDTupperCase: TcxEditRepositoryMaskItem;
    cxEDTfindSAP: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    FMillDescription: string;
    { Private declarations }
    procedure m_SetFindMill(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_InitializeData(DataSet: TDataSet);
    procedure SetMillDescription(const Value: string);
    procedure m_SetFindSAP(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeSAP(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    property MillDescription : string read FMillDescription write SetMillDescription; // guarda a descri��o em Portugu�s do c�digo fabril
  end;

var
  FRAMEpaymentMill: TFRAMEpaymentMill;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFREditSOA, 
  kneFGFindUtils, Global,
  //---
  MillServiceUtils, SapPaymentServiceUtils;

{$R *.dfm}

{ TFRAMEpaymentMill }

constructor TFRAMEpaymentMill.Create(AOwner: TComponent);
begin
  inherited;

	// SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'paymentCode=gpsPayment';
  DataPacketName := 'LnkPayment';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');

    // Ordem Campos ............................................................
    DefineOrderFields('mill;millPayment;millDescrip;sapPayment;stat');

    // Key Fields ..............................................................
    KeyFields:= 'mill;millPayment';

    // Custom Editors ..........................................................
    AddCustomField('mill', 'cxEDTfind');
    AddCustomField('stat', 'cxEDTstat');
    AddCustomField('millPayment', 'cxEDTupperCase');
    AddCustomField('millDescrip', 'cxEDTupperCase');
    AddCustomField('sapPayment', 'cxEDTfindSAP');      //JAR #10002  05-07-2011
//    AddCustomField('sapPayment', 'cxEDTupperCase');  //JAR #10002  05-07-2011
    ColsWidthInGrid := '60;100;200;100;100';
  end; //with
               
	// Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_SetFindMill;
  cxEDTfindSAP.Properties.OnButtonClick := m_SetFindSAP;

end;

procedure TFRAMEpaymentMill.m_InitializeData(DataSet: TDataSet);
var
	lv_PayCode : string;
begin
```

#### **FRpaymentMill.dfm**

```
inherited FRAMEpaymentMill: TFRAMEpaymentMill
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTupperCase: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTfindSAP: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

