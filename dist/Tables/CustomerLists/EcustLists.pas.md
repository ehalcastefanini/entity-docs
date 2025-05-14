<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário chamado `TFORMEcustLists` que gerencia listas de clientes. Ele permite a exibição e edição de dados relacionados a clientes, incluindo a funcionalidade de relacionamento mestre-detalhe entre diferentes conjuntos de dados. O formulário também inclui validações e ações específicas, como a exclusão de códigos de clientes ao alterar o mercado selecionado.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL - Visual Component Library).
  - Componentes personalizados como `kneCBEdit`, `kneFRGridEditSOA`, `kneFREditSOA`, entre outros.
  - Componentes visuais como `TsPanel`, `TcxGrid`, `TsDBComboBox`, e `TsDBText`.

* **Tipo de Formulário:**  
  - **Grid Display:**  
    - **Colunas do Grid:**  
      - `cxDBG` (Grid principal para exibição de dados detalhados).  
    - **Ações do Grid:**  
      - Exclusão de registros detalhados ao alterar o mercado selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**  
  - Alterar o mercado selecionado e, se necessário, excluir os códigos de clientes associados.
  - Exibir e editar dados mestre-detalhe relacionados a listas de clientes.

* **Componentes Principais:**  
  - `FRAMEcustLists1`: Gerencia a seleção de mercado e outras informações relacionadas.
  - `FRAMEcustListsDetail1`: Exibe os detalhes dos clientes em um grid.
  - `PNLdata`: Painel principal que organiza os componentes visuais.

* **Pseudo-código das Ações e Eventos:**  
  - Evento `OnClick` do botão Cancelar:  
    ```pseudo
    se botão Cancelar for clicado então fechar o formulário.
    ```
  - Evento `OnChange` do mercado:  
    ```pseudo
    se o mercado selecionado for alterado então
        se o usuário confirmar a exclusão então
            excluir todos os registros detalhados.
        senão
            restaurar o valor anterior do mercado.
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. O formulário é inicializado com a função `m_CreateFormEdit`.
  2. Os dados são carregados com a função `m_getData`, que configura a relação mestre-detalhe.
  3. O usuário pode interagir com os componentes, como alterar o mercado ou editar os dados no grid.
  4. Alterações no mercado disparam o evento `m_OnChangeMkt`, que pode excluir registros detalhados.

* **Dados Necessários:**  
  - Mercado selecionado no componente `FRAMEFindMarket`.
  - Dados detalhados exibidos no grid `cxDBG`.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - Alterar mercado: O mercado deve ser selecionado no componente `FRAMEFindMarket`.
  - Exclusão de registros: O usuário deve confirmar a exclusão ao alterar o mercado.

* **Filtros Disponíveis:**  
  - Filtro de mercado no componente `FRAMEFindMarket`.

* **Mensagens de Erro:**  
  - "Cust codes will be deleted. Are you Sure?" ao alterar o mercado.

* **Valores Padrão dos Campos:**  
  - Não especificado no código.

* **Validações e Condições dos Campos:**  
  - O mercado selecionado deve ser validado para evitar inconsistências.

---

## 5. Funções Principais:

* **`m_CreateFormEdit`:**  
  Cria e inicializa o formulário `TFORMEcustLists`.

* **`m_getData`:**  
  Carrega os dados e configura a relação mestre-detalhe.

* **`m_OnChangeMkt`:**  
  Gerencia a lógica de alteração do mercado, incluindo a exclusão de registros detalhados.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos especificadas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `kneCBEdit`, `kneFRGridEditSOA`, `kneFREditSOA`, entre outros.

* **Componentes Personalizados:**  
  - `TFRAMEcustLists` e `TFRAMEcustListsDetail`.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**  
  - `FRAMEFindMarket` (tipo: string, obrigatório, não especificado no código).
  - `cxDBG` (grid para exibição de dados detalhados).

* **Mapeamento de Valores e Colunas do Banco de Dados:**  
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  var
    Form: TFORMEcustLists;
  begin
    Form := TFORMEcustLists.Create(Application);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```

* **HTML Renderizado:**  
  ```html
  <div style="width: 784px; border: 1px solid #ccc; font-family: Verdana;">
    <div style="height: 155px; border-bottom: 1px solid #ccc;">
      <label for="market">Market:</label>
      <input id="market" type="text" style="width: 100%;">
    </div>
    <div style="height: 326px;">
      <table style="width: 100%; border-collapse: collapse;">
        <thead>
          <tr>
            <th style="border: 1px solid #ccc;">Column 1</th>
            <th style="border: 1px solid #ccc;">Column 2</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td style="border: 1px solid #ccc;">Data 1</td>
            <td style="border: 1px solid #ccc;">Data 2</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* **Comentário NAVOPTECH2022-4707:**  
  Indica a lógica de exclusão de códigos de clientes ao alterar o mercado.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar listas de clientes com uma relação mestre-detalhe. Ele é bem estruturado, mas poderia ser melhorado com validações mais robustas e mensagens de erro mais detalhadas. A dependência de componentes personalizados pode dificultar a manutenção.

---

## 13. Resumo Curto:

O formulário `TFORMEcustLists` gerencia listas de clientes com suporte a relações mestre-detalhe e validações ao alterar o mercado. Ele utiliza componentes personalizados e oferece funcionalidades básicas de edição e exclusão de dados.#### **EcustLists.pas**

```
unit EcustLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFRGridEditSOA, FRcustListsDetail, kneFREditSOA,
  kneFRCtrlEditSOA, FRcustLists;

type
  TFORMEcustLists = class(TFORMkneBaseEdit)
    PNLdata: TsPanel;
    FRAMEcustLists1: TFRAMEcustLists;
    FRAMEcustListsDetail1: TFRAMEcustListsDetail;
    procedure BTCancelClick(Sender: TObject);

  protected
    procedure m_getData; override;

  private
    { Private declarations }
    FMktValue: String;
    procedure m_OnChangeMkt(Sender: TObject);
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEcustLists: TFORMEcustLists;

implementation

uses
  kneUtils;

{$R *.dfm}

{ TFORMEcustLists }

class function TFORMEcustLists.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
  Result := TFORMEcustLists.Create(Application);
end;

procedure TFORMEcustLists.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;

  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));
  // setup das rela��es master-detail
  FRAMEcustListsDetail1.MasterSource := lv_MasterFrame.DStable;

  inherited m_getData;

  FRAMEcustLists1.OnChangeMkt := m_OnChangeMkt;
  FMktValue := FRAMEcustLists1.FRAMEFindMarket.Text;

end;

//NAVOPTECH2022-4707 (cmosilva 20-07-2023)
procedure TFORMEcustLists.m_OnChangeMkt(Sender: TObject);
begin

  with FRAMEcustListsDetail1 do
  begin

    if not CDStable.IsEmpty then
    begin
      if not SameText(FRAMEcustLists1.FRAMEFindMarket.Text, FMktValue) then
      begin
        if MessageDlg('Cust codes will be deleted. Are you Sure?', mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin

            if (not CDStable.Active) or (CDStable.IsEmpty) then Exit;

            CDStable.DisableControls;
            try
              CDStable.First;

              while not CDStable.Eof do
              begin
                CDStable.Delete;

              end; //while

            finally
              if CDStable.ControlsDisabled then
                CDStable.EnableControls;
            end;

        end else
        begin

          FRAMEcustLists1.FRAMEFindMarket.SetValue(FMktValue);
```

#### **EcustLists.dfm**

```
inherited FORMEcustLists: TFORMEcustLists
  Left = 640
  Top = 323
  Height = 562
  Caption = 'Customer Lists'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLdata: TsPanel [2]
    Left = 0
    Top = 41
    Width = 784
    Height = 483
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEcustLists1: TFRAMEcustLists
      Left = 1
      Top = 1
      Width = 782
      Height = 155
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBL2: TsLabel
        FocusControl = FRAMEcustLists1.FRAMEFindMarket.DBE
      end
      inherited LBLbusUnit: TsLabel
        FocusControl = FRAMEcustLists1.FRAMEBusUnit1.DBCBObusUnit
      end
      inherited PNLfooter: TsPanel
        Top = 121
        Width = 782
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 79
        Width = 782
        inherited GRPstatus: TsGroupBox
          Width = 782
          inherited DBTXTlastUpd: TsDBText
            DataSource = FRAMEcustLists1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            DataSource = FRAMEcustLists1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            DataBinding.DataSource = FRAMEcustLists1.DStable
            Width = 97
          end
        end
      end
      inherited FRAMEBusUnit1: TFRAMEBusUnit
        inherited DBCBObusUnit: TsDBComboBox
          DataSource = FRAMEcustLists1.DStable
        end
      end
    end
    inline FRAMEcustListsDetail1: TFRAMEcustListsDetail
      Left = 1
      Top = 156
      Width = 782
      Height = 326
      Align = alClient
      ParentBackground = False
      TabOrder = 1
      inherited cxDBG: TcxGrid
        Width = 782
        Height = 292
      end
      inherited PNLfooter: TsPanel
        Top = 292
        Width = 782
      end
    end
  end
  inherited IMLbuttons: TImageList
    Bitmap = {
      494C01010C000E00040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000006000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

