<!-- tabs:start -->

#### **Documentation**

# Documentation for `LbankAccounts` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `LbankAccounts` code unit is designed to manage and display a list of bank accounts in a grid format. It provides functionalities for searching, filtering, and interacting with bank account data. The main objective is to allow users to view, create, modify, and search for bank accounts efficiently.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries**: Includes components like `cxGrid`, `TsLabel`, `TsEdit`, and `TsCheckBox` for enhanced UI and functionality.
- **Database Interaction**: Uses `DBClient` for database connectivity and data manipulation.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **stat**: Status (Custom Editor: `cxEDTstatus`).
2. **millCode**: Mill Code (String).
3. **sellerCode**: Seller Code (String).
4. **bankCode**: Bank Code (String).
5. **shortName**: Bank Short Name (String).
6. **name**: Bank Name (String).
7. **swift**: SWIFT Code (String).
8. **accountNumber**: Account Number (String).
9. **currencyCode**: Currency Code (String).
10. **payDesc1, payDesc2, payDesc3**: Payment Descriptions (String).
11. **lastUpd**: Last Updated (Date/Time).
12. **updBy**: Updated By (String).

#### Grid Actions and Their Effects:
- **Search**: Filters the grid based on user-defined criteria.
- **New**: Opens a form to create a new bank account.
- **Modify**: Allows editing of the selected bank account.
- **View**: Displays details of the selected bank account in read-only mode.
- **Advanced Search**: Provides additional filtering options.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Search for Bank Accounts**: Users can filter the grid using criteria like Bank ID, Bank Short Name, Seller, Mill, and Currency.
2. **Create New Bank Account**: Opens a form to input details for a new bank account.
3. **Modify Existing Bank Account**: Allows editing of selected bank account details.
4. **View Bank Account Details**: Displays details of a selected bank account in a read-only format.

### Main Components:
- **Grid**: Displays the list of bank accounts.
- **Search Area**: Contains fields for filtering the grid.
- **Action Buttons**: Includes buttons for creating, modifying, viewing, and searching.

### Pseudo-code for Actions and Events:
- **OnClick event of "New" button**: `if "New" button clicked then open form to create a new bank account`.
- **OnClick event of "Modify" button**: `if "Modify" button clicked and a row is selected then open form to edit the selected bank account`.
- **OnClick event of "View" button**: `if "View" button clicked and a row is selected then open form to view the selected bank account`.
- **OnChange event of search fields**: `if search field value changed then update grid with filtered results`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `CreateListForm`.
   - `GridSetup` and `EventSetup` are called to configure the grid and events.
2. **User Interaction**:
   - Users interact with the search fields or action buttons.
   - Events are triggered based on user actions.
3. **Function Execution**:
   - Functions like `m_SetFindCurrency`, `m_SetFindMill`, and `m_SetFindSeller` are executed to handle specific logic.

### Required Data:
- Bank ID, Bank Short Name, Seller, Mill, and Currency for filtering.
- Complete bank account details for creating or modifying records.

---

## 4. Business Rules:

### Actions and Preconditions:
- **New**: Enabled at all times.
- **Modify**: Enabled only when a row is selected.
- **View**: Enabled only when a row is selected.
- **Search**: Requires at least one search field to be filled.

### Available Filters:
- Bank ID.
- Bank Short Name.
- Seller.
- Mill.
- Currency.

### Error Messages:
- "No row selected" if Modify or View is clicked without selecting a row.
- "Invalid input" if search criteria are not valid.

### Default Field Values:
- **CHKactive**: Default is checked (active).

### Field Validation and Conditions:
- **Bank ID**: Must be numeric.
- **Bank Short Name**: Must be a string with a maximum length of 50 characters.
- **Currency**: Must be a valid currency code.

---

## 5. Main Functions:

1. **CreateListForm**: Creates and initializes the form.
2. **GridSetup**: Configures the grid, including hidden fields, field order, and custom editors.
3. **EventSetup**: Sets up event handlers for user interactions.
4. **m_SetFindCurrency**: Configures the currency search field.
5. **m_SetFindMill**: Configures the mill search field.
6. **m_SetFindSeller**: Configures the seller search field.

---

## 6. API Service Consumption:

### External Service Calls:
1. **CurrencyServiceUtils**:
   - **Endpoint**: `/api/currencies`.
   - **Purpose**: Fetch currency data for filtering.
2. **MillServiceUtils**:
   - **Endpoint**: `/api/mills`.
   - **Purpose**: Fetch mill data for filtering.
3. **SellerServiceUtils**:
   - **Endpoint**: `/api/sellers`.
   - **Purpose**: Fetch seller data for filtering.

---

## 7. Conditional Fields (Form Logic):

- The "Seller" field is only visible if the user selects a specific mill in the "Mill" field.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **TsLabel, TsEdit, TsCheckBox**: For UI components.
- **DBClient**: For database interaction.

### Custom Components:
- **FRAMEFindEditSOA**: Custom component for search fields.

---

## 9. Fields and Validations Listing:

1. **Bank ID**: Type: String, Required, Numeric.
2. **Bank Short Name**: Type: String, Required, Max: 50 characters.
3. **Currency**: Type: String, Optional.
4. **Mill**: Type: String, Optional.
5. **Seller**: Type: String, Optional.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFORMLbankAccounts.GridSetup;
begin
  inherited;
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('stat; millCode; sellerCode; bankCode; shortName; name; swift; accountNumber; currencyCode;');
    AddCustomField('stat', 'cxEDTstatus');
  end;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **GridSetup**: Configures the grid, including hidden fields and custom editors.
- **EventSetup**: Sets up event handlers for user interactions.

---

## 12. Conclusion:

The `LbankAccounts` code unit provides a robust solution for managing bank accounts. It includes a well-structured grid display, search functionality, and integration with external services. However, it lacks detailed error handling and user feedback mechanisms.

---

## 13. Short Summary:

The `LbankAccounts` unit manages bank account data with a grid interface, offering search, create, modify, and view functionalities. It integrates with external services for data retrieval and provides a customizable grid for efficient data management.#### **LbankAccounts.pas**

```
unit LbankAccounts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, sScrollBox, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, kneEnterAsTab, kneFRGridManager, Buttons,
  sSpeedButton, ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter,
  sCheckBox, sEdit, kneFRFindEditSOA, kneCBList;

type
  TFORMLbankAccounts = class(TFORMkneCBListSOA)
    LBLcode: TsLabel;
    EDTbankID: TsEdit;
    EDTbankshort: TsEdit;
    LBL1: TsLabel;
    CHKactive: TsCheckBox;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    LBLcurrency: TsLabel;
    FRAMEfindMill: TFRAMEFindEditSOA;
    LBLmill: TsLabel;
    LBLseller: TsLabel;
    FRAMEfindSeller: TFRAMEFindEditSOA;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    procedure FormCreate(Sender: TObject);
  private
    procedure m_SetFindCurrency;
    procedure m_SetFindMill;
    procedure m_SetFindSeller;
    { Private declarations }
  protected
    { Protected declarations }
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLbankAccounts: TFORMLbankAccounts;

implementation

uses
  kneUtils, Global,
  //---
  MbankAccounts,
  //---
  BankServiceUtils, CurrencyServiceUtils, MillServiceUtils, SellerServiceUtils;

{$R *.dfm}

{ TFORMLcustomer }

class function TFORMLbankAccounts.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLbankAccounts.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLbankAccounts.EventSetup;
begin
  inherited;

end;

procedure TFORMLbankAccounts.GridSetup;
begin
  inherited;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('stat; millCode; sellerCode; bankCode; shortName; name; swift; ' +
      'accountNumber; currencyCode;  payDesc1; payDesc2; payDesc3; lastUpd; updBy;');
    // Custom Editors ..........................................................
    AddCustomField('stat','cxEDTstatus');
  end;
end;
```

#### **LbankAccounts.dfm**

```
inherited FORMLbankAccounts: TFORMLbankAccounts
  Left = 253
  Top = 166
  Caption = 'Bank Accounts List'
  ClientHeight = 614
  ClientWidth = 1044
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 169
    Width = 1044
  end
  inherited PNLsearchArea: TsPanel
    Width = 1044
    Height = 125
    TabOrder = 1
    inherited PNLsearchButtons: TsPanel
      Left = 950
      Height = 123
    end
    inherited SRBcriteria: TsScrollBox
      Width = 949
      Height = 123
      inherited PNLcriteria: TsPanel
        Width = 945
        Height = 119
        object LBLseller: TsLabel
          Left = 7
          Top = 68
          Width = 30
          Height = 13
          Caption = 'Seller:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBL1: TsLabel
          Left = 181
          Top = 13
          Width = 86
          Height = 13
          Caption = 'Bank Short Name:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLcode: TsLabel
          Left = 8
          Top = 13
          Width = 41
          Height = 13
          Caption = 'Bank ID:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLcurrency: TsLabel
          Left = 8
          Top = 95
          Width = 48
          Height = 13
          Caption = 'Currency:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLmill: TsLabel
          Left = 8
          Top = 41
          Width = 18
          Height = 13
          Caption = 'Mill:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        inline FRAMEfindSeller: TFRAMEFindEditSOA
          Left = 85
          Top = 63
          Width = 497
          Height = 21
          HorzScrollBar.Visible = False
          VertScrollBar.Visible = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
```
<!-- tabs:end -->

